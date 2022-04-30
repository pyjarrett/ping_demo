with Ada.Text_IO;
with Interfaces;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Networking.ICMP is
    subtype int is Interfaces.C.int;
    use type int;

    subtype socklen_t is int;
    subtype Socket_Type is int;

    -- /*
    --  * Types
    --  */
    -- #define SOCK_STREAM     1               /* stream socket */
    -- #define SOCK_DGRAM      2               /* datagram socket */
    -- #define SOCK_RAW        3               /* raw-protocol interface */
    -- #if !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)
    -- #define SOCK_RDM        4               /* reliably-delivered message */
    -- #endif  /* (!_POSIX_C_SOURCE || _DARWIN_C_SOURCE) */
    -- #define SOCK_SEQPACKET  5               /* sequenced packet stream */
    SOCK_RAW : constant Socket_Type := 3;

    -- /usr/include/sys/sockets.h
    -- struct sockaddr {
    -- 	__uint8_t       sa_len;         /* total length */
    -- 	sa_family_t     sa_family;      /* [XSI] address family */
    -- 	char            sa_data[14];    /* [XSI] addr value (actually larger) */
    -- };
    subtype sockaddr_ptr is System.Storage_Elements.Integer_Address;

    -- /usr/include/sys/_types/_sa_family_t.h
    -- typedef __uint8_t               sa_family_t;

    subtype addrinfo_ptr is System.Storage_Elements.Integer_Address;
    type ai_flags_t is mod 2 ** Interfaces.C.int'Size;

    type sa_data_t is array (Natural range 0 .. 14) of Character with Convention => C;

    type sockaddr is record
        sa_len    : Interfaces.Unsigned_8;
        sa_family : Interfaces.Unsigned_8;
        sa_data   : sa_data_t;
    end record;

    -- /usr/include/sys/_types/_socklen_t.h
    -- typedef __darwin_socklen_t      socklen_t;

    -- /usr/include/arm/_types.h
    -- typedef __uint32_t              __darwin_socklen_t;     /* socklen_t (duh) */

    -- from /usr/include/netdb.h
    --
    -- struct addrinfo {
    -- 	int	ai_flags;	/* AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST */
    -- 	int	ai_family;	/* PF_xxx */
    -- 	int	ai_socktype;	/* SOCK_xxx */
    -- 	int	ai_protocol;	/* 0 or IPPROTO_xxx for IPv4 and IPv6 */
    -- 	socklen_t ai_addrlen;	/* length of ai_addr */
    -- 	char	*ai_canonname;	/* canonical name for hostname */
    -- 	struct	sockaddr *ai_addr;	/* binary address */
    -- 	struct	addrinfo *ai_next;	/* next structure in linked list */
    -- };
    type addrinfo is record
        ai_flags     : Interfaces.C.int := 0;    -- AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
	    ai_family    : ai_flags_t := 0;        -- PF_xxx
	 	ai_socktype  : Socket_Type := 0; -- SOCK_xxx
        ai_protocol  : Interfaces.C.int := 0; -- /* 0 or IPPROTO_xxx for IPv4 and IPv6 */
        ai_addrlen   : socklen_t := 0; --	length of ai_addr */
        ai_canonname : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;	-- canonical name for hostname */
        ai_addr      : sockaddr_ptr := 0;	-- binary address */
        ai_next      : addrinfo_ptr := 0;	-- next structure in linked list */
    end record
        with Pack, Convention => C;

    -- TODO: Set these values.
    AF_UNSPEC : constant ai_flags_t := 0;
    IPPROTO_ICMP : constant int := 0;

    AI_PASSIVE : constant := 16#00000001#;
    AI_CANONNAME : constant := 16#00000002#;

    function Make_Hint_ICMP_V4
        return addrinfo
    is
    begin
        return Result : addrinfo do
            Result.ai_family   := AF_UNSPEC;
            Result.ai_socktype := SOCK_RAW;
            Result.ai_protocol := IPPROTO_ICMP; -- or IPPROTO_ICMPV6
        end return;
    end Make_Hint_ICMP_V4;

    -- int		getaddrinfo(const char * __restrict,
    --              const char * __restrict,
    -- 			    const struct addrinfo * __restrict,
    -- 			    struct addrinfo ** __restrict);
    -- getaddrinfo(target, nullptr, &hints, &m_alternatives);
    function getaddrinfo(
        Target : Interfaces.C.Strings.chars_ptr;
        Unused : Interfaces.C.Strings.chars_ptr;
        Hints  : addrinfo_ptr;
        Result : System.Address
    ) return Interfaces.C.Int
        with Import, Convention => C;

    subtype Socket_Descriptor is int;
    Invalid_Socket : constant Socket_Descriptor := -1;

    subtype Protocol_Family is int;

    -- man socket
    --
    --  #include <sys/socket.h>
    --  int socket(int domain, int type, int protocol);
    --
    -- A -1 is returned if an error occurs, otherwise the return value is a
    -- descriptor referencing the socket.
    function socket(
        Communication_Domain    : Protocol_Family;
        Communication_Semantics : Socket_Type;
        Protocol                : int
    ) return Socket_Descriptor
        with Import, Convention => C;

    subtype Connect_Status is int;
    Connect_Error   : constant Connect_Status := -1;
    Connect_Success : constant Connect_Status := 0;

    -- int connect(int socket,
    --             const struct sockaddr *address,
    --             socklen_t address_len);
    function connect (
        Socket         : Socket_Descriptor;
        Address        : sockaddr_ptr;
        Address_Length : socklen_t
    ) return Connect_Status
        with Import, Convention => C;

    function close (File_Descriptor : int) return int
        with Import, Convention => C;

    -- Hide the return value of close() when we don't need it.
    procedure close (File_Descriptor : int) is
        Unused : int;
    begin
        pragma Unreferenced (Unused);
        Unused := close (File_Descriptor);
    end close;

    -- __BEGIN_DECLS
    -- extern int * __error(void);
    -- #define errno (*__error())
    -- __END_DECLS
    function errno return Interfaces.Integer_64
        with Import, Convention => C, External_Name => "__error";

    -- Pings a host, reporting status to the user.
    procedure Ping(Host : String)
    is
        Host_CStr : aliased Interfaces.C.char_array := Interfaces.C.To_C (host);
        Hints : constant addrinfo := Make_Hint_ICMP_V4;
        Address_Infos : System.Address;
        use type Interfaces.C.int;
        use type Interfaces.C.Strings.chars_ptr;

        package Addrinfo_Conversions is new System.Address_To_Access_Conversions (Object => addrinfo);

        addr_info_access : access addrinfo := null;
    begin
        -- Gets the address to look up.
        if getaddrinfo (
            Interfaces.C.Strings.To_Chars_Ptr (Host_CStr'Unchecked_Access),
            Interfaces.C.Strings.Null_Ptr,
            System.Storage_Elements.To_Integer (Hints'Address),
            Address_Infos'Address) /= 0
        then
            -- TODO: Write to STDERR
            Ada.Text_IO.Put_Line ("Unable to find address to look up");
            return;
        end if;

        addr_info_access := Addrinfo_Conversions.To_Pointer (Address_Infos);
        Ada.Text_IO.Put_Line (Interfaces.C.int'Image(addr_info_access.all.ai_addrlen));

        if addr_info_access.ai_canonname = Interfaces.C.Strings.Null_Ptr then
            Ada.Text_IO.Put_Line ("Null canonical name string");
        end if;

        declare
            Client_Socket : Socket_Descriptor := socket (
                int(addr_info_access.ai_family),
                addr_info_access.ai_socktype,
                addr_info_access.ai_protocol
            );
            Connect_Result : Connect_Status;
        begin
            if Client_Socket = Invalid_Socket then
                Ada.Text_IO.Put_Line ("Unable to create socket.");
                Ada.Text_IO.Put_Line (Interfaces.Integer_64'image(errno));
                return;
            else
                Ada.Text_IO.Put_Line ("Created the send socket.");
            end if;

            Connect_Result := connect (Client_Socket, addr_info_access.ai_addr, addr_info_access.ai_addrlen);
            if Connect_Result /= Connect_Success then
                Ada.Text_IO.Put_Line ("Unable to connect to socket:" & Connect_Status'Image (Connect_Result));
                close (Client_Socket);
                Client_Socket := Invalid_Socket;
                return;
            end if;
        end;

    --     EchoRequest echoRequest(1, 1);
    --     int sendResult = send(clientSocket, reinterpret_cast<char*>(&echoRequest), sizeof(echoRequest), 0);
    --     if (sendResult == SOCKET_ERROR) {
    -- #if _WIN32
    --         logError() << "Unable to send " << WSAGetLastError() << '\n';
    -- #elif __APPLE__
    --         logError() << "Unable to send " << errno << '\n';
    -- #endif
    --         logError() << "Are you sending as an administrator?\n";
            
    -- #if _WIN32
    --         closesocket(clientSocket);
    -- #elif __APPLE__
    --         close(clientSocket);
    -- #endif
    --         return false;
    --     }
    --     logInfo() << "Sent: " << sendResult << " bytes\n";

    --     std::vector<char> buffer;
    --     buffer.resize(1024);
    --     int recvResult = recv(clientSocket, buffer.data(), (int)buffer.size(), 0);
    --     if (recvResult > 0) {
    --         logInfo() << "Received " << recvResult << " bytes\n";
    --     } else if (recvResult == 0) {
    --         logInfo() << "Socket closed\n";
    --     } else {
    -- #if _WIN32
    --         logError() << "Receive failed: " << WSAGetLastError() << '\n';
    -- #elif __APPLE__
    --         logError() << "Receive failed: " << errno << '\n';
    -- #endif
        -- }


        Ada.Text_IO.Put_Line ("Pinging: " & Host);
        Ada.Text_IO.Put_Line (Integer'Image(Hints'Size));


    end Ping;

end Networking.ICMP;
