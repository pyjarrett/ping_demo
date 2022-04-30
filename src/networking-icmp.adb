with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Interfaces;
with System.Address_Image;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Networking.ICMP is
    package TIO renames Ada.Text_IO;

    subtype chars_ptr is Interfaces.C.Strings.chars_ptr;
    subtype int is Interfaces.C.int;
    use type int;

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
    subtype Socket_Type is int;
    SOCK_RAW : constant Socket_Type := 3;

    -- /usr/include/sys/sockets.h
    -- struct sockaddr {
    -- 	__uint8_t       sa_len;         /* total length */
    -- 	sa_family_t     sa_family;      /* [XSI] address family */
    -- 	char            sa_data[14];    /* [XSI] addr value (actually larger) */
    -- };
    --
    -- /usr/include/sys/_types/_sa_family_t.h
    -- typedef __uint8_t               sa_family_t;
    type sa_data_t is array (Natural range 0 .. 13) of Character with Convention => C;
    type sockaddr is record
        sa_len    : Interfaces.Unsigned_8;
        sa_family : Interfaces.Unsigned_8;
        sa_data   : sa_data_t;
    end record;

    package Socket_Address_Conversions is new System.Address_To_Access_Conversions(sockaddr);
    subtype sockaddr_ptr is Socket_Address_Conversions.Object_Pointer;
    pragma Assert(sockaddr_ptr'Size = 64);

    function Image (Ptr : Socket_Address_Conversions.Object_Pointer) return String
    is
        use Ada.Characters.Latin_1;
        Data : constant sa_data_t := Ptr.sa_data;
    begin
        return Interfaces.Unsigned_8'Image (Ptr.sa_len) & LF
            & Interfaces.Unsigned_8'Image (Ptr.sa_family) & LF
            & Data (0)'Image
            & Data (1)'Image
            & Data (2)'Image
            & Data (3)'Image
            & Data (4)'Image
            & Data (5)'Image
            & Data (6)'Image
            & Data (7)'Image
            & Data (8)'Image
            & Data (9)'Image
            & Data (10)'Image
            & Data (11)'Image
            & Data (12)'Image
            & Data (13)'Image;
    end Image;

    subtype addrinfo_ptr is System.Storage_Elements.Integer_Address;
    type ai_flags_t is mod 2 ** int'Size;

    -- /usr/include/sys/_types/_socklen_t.h
    -- typedef __darwin_socklen_t      socklen_t;
    subtype socklen_t is int;

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
        ai_flags     : int          := 0;
	    ai_family    : ai_flags_t   := 0;
	 	ai_socktype  : Socket_Type  := 0;
        ai_protocol  : int          := 0;
        ai_addrlen   : socklen_t    := 0;
        ai_canonname : chars_ptr    := Interfaces.C.Strings.Null_Ptr;
        ai_addr      : System.Address := System.Null_Address;
        ai_next      : System.Address := System.Null_Address;
    end record
        with Convention => C;

    function Image (Address : System.Address) return String is (System.Address_Image (Address));
    function Image (Self : addrinfo) return String is
        use Ada.Characters.Latin_1;
    begin
        return int'Image (Self.ai_flags) & LF
            & " family:   " & ai_flags_t'Image (Self.ai_family) & LF
            & " socktype: " & Socket_Type'Image (Self.ai_socktype) & LF
            & " protocol: " & int'Image (Self.ai_protocol) & LF
            & " addrlen:  " & socklen_t'Image (Self.ai_addrlen) & LF
            & " address:  " & Image (Self.ai_addr) & LF
            & " next:     " & Image (Self.ai_next);
    end Image;

    -- /usr/include/sys/socket.h
    -- #define AF_UNSPEC       0               /* unspecified */
    -- #define AF_UNIX         1               /* local to host (pipes) */
    -- #if !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)
    -- #define AF_LOCAL        AF_UNIX         /* backward compatibility */
    AF_UNSPEC : constant ai_flags_t := 0;

    -- /usr/include/netinet/in.h
    -- #define IPPROTO_IP              0               /* dummy for IP */
    -- #if !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)
    -- #define IPPROTO_HOPOPTS 0               /* IP6 hop-by-hop options */
    -- #endif  /* (!_POSIX_C_SOURCE || _DARWIN_C_SOURCE) */
    -- #define IPPROTO_ICMP            1               /* control message protocol */
    IPPROTO_IP   : constant int := 0;
    IPPROTO_ICMP : constant int := 1;

    -- #define	AI_PASSIVE	0x00000001 /* get address to use bind() */
    -- #define	AI_CANONNAME	0x00000002 /* fill ai_canonname */
    -- #define	AI_NUMERICHOST	0x00000004 /* prevent host name resolution */
    -- #define	AI_NUMERICSERV	0x00001000 /* prevent service name resolution */
    AI_PASSIVE   : constant := 16#00000001#;
    AI_CANONNAME : constant := 16#00000002#;

    function Make_Hint_ICMP_V4 return addrinfo is
    begin
        return (
            ai_family   => AF_UNSPEC,
            ai_socktype => SOCK_RAW,
            ai_protocol => IPPROTO_ICMP, -- or IPPROTO_ICMPV6
            others      => <>
        );
    end Make_Hint_ICMP_V4;

    --  int getaddrinfo(
    --      const char *hostname,
    --      const char *servname,
    --      const struct addrinfo *hints,
    --      struct addrinfo **res);
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
        Address        : System.Address;
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

    function Get_Errno_String return String
    is
        -- __BEGIN_DECLS
        -- extern int * __error(void);
        -- #define errno (*__error())
        -- __END_DECLS
        function errno return System.Address
            with Import, Convention => C, External_Name => "__error";

        --  char* strerror(int errnum);
        function strerror (errnum : int) return Interfaces.C.Strings.chars_ptr
            with Import, Convention => C;

        package Int_Conversions is new System.Address_To_Access_Conversions(int);
        Errno_Address : constant System.Address := errno;
        Errno_Ptr     : constant access int := Int_Conversions.To_Pointer (Errno_Address);
        Errno_Str     : constant String := Interfaces.C.Strings.Value (strerror (Errno_Ptr.all));
    begin
        return Errno_Str;
    end Get_Errno_String;

    -- Pings a host, reporting status to the user.
    procedure Ping(Host : String)
    is
        Host_CStr     : aliased Interfaces.C.char_array := Interfaces.C.To_C (host);
        Hints         : constant addrinfo := Make_Hint_ICMP_V4;

        use type Interfaces.C.Strings.chars_ptr;
        package Addrinfo_Conversions is new System.Address_To_Access_Conversions (Object => addrinfo);

        Address_Infos : Addrinfo_Conversions.Object_Pointer := null;
        Success : constant := 0;
    begin
        -- Gets the address to look up.
        if getaddrinfo (
            Interfaces.C.Strings.To_Chars_Ptr (Host_CStr'Unchecked_Access),
            Interfaces.C.Strings.Null_Ptr,
            System.Storage_Elements.To_Integer (Hints'Address),
            Address_Infos'Address) /= Success
        then
            -- TODO: Write to STDERR
            Ada.Text_IO.Put_Line ("Unable to find address to look up");
            return;
        end if;

        if Address_Infos.ai_canonname = Interfaces.C.Strings.Null_Ptr then
            Ada.Text_IO.Put_Line ("Null canonical name string");
        end if;

        declare
            Client_Socket : Socket_Descriptor := socket (
                int(Address_Infos.ai_family),
                Address_Infos.ai_socktype,
                Address_Infos.ai_protocol
            );
            Connect_Result : Connect_Status;
        begin
            if Client_Socket = Invalid_Socket then
                Ada.Text_IO.Put_Line ("Unable to create socket.");
                Ada.Text_IO.Put_Line (Get_Errno_String);
                return;
            else
                Ada.Text_IO.Put_Line ("Created the send socket.");
            end if;

            Connect_Result := connect (Client_Socket, Address_Infos.ai_addr, Address_Infos.ai_addrlen);
            if Connect_Result /= Connect_Success then
                Ada.Text_IO.Put_Line ("Unable to connect to socket:" & Connect_Status'Image (Connect_Result));
                Ada.Text_IO.Put_Line ("Socket Error: " & Get_Errno_String);
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
