with Interfaces.C;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package Networking.Sockets is
    use type Interfaces.C.int;

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\ws2def.h
    -- //
    -- // Socket types.
    -- //
    -- #define SOCK_STREAM     1
    -- #define SOCK_DGRAM      2
    -- #define SOCK_RAW        3
    -- #define SOCK_RDM        4
    -- #define SOCK_SEQPACKET  5
    
    subtype Socket_Type is int;
    SOCK_RAW : constant Socket_Type := 3;

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\ws2def.h
    --
    -- typedef unsigned short USHORT;
    -- typedef USHORT ADDRESS_FAMILY;
    --
    -- //
    -- // Structure used to store most addresses.
    -- //
    -- typedef struct sockaddr {
    --
    -- #if (_WIN32_WINNT < 0x0600)
    --     u_short sa_family;
    -- #else
    --     ADDRESS_FAMILY sa_family;           // Address family.
    -- #endif //(_WIN32_WINNT < 0x0600)
    --
    --     CHAR sa_data[14];                   // Up to 14 bytes of direct address.
    -- } SOCKADDR, *PSOCKADDR, FAR *LPSOCKADDR;
    subtype Address_Family is Interfaces.C.short;
    type sa_data_t is array (Natural range 0 .. 13) of Character with Convention => C;
    type sockaddr is record
        sa_family : Address_Family;
        sa_data   : sa_data_t;
    end record;

    package Socket_Address_Conversions is new System.Address_To_Access_Conversions(sockaddr);
    subtype sockaddr_ptr is Socket_Address_Conversions.Object_Pointer;
    pragma Assert(sockaddr_ptr'Size = 64);

    subtype addrinfo_ptr is System.Storage_Elements.Integer_Address;
    type ai_flags_t is mod 2 ** int'Size;

    --   C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\ws2def.h  
    -- //
    -- //  Structure used in getaddrinfo() call
    -- //
    -- typedef struct addrinfo
    -- {
    --     int                 ai_flags;       // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    --     int                 ai_family;      // PF_xxx
    --     int                 ai_socktype;    // SOCK_xxx
    --     int                 ai_protocol;    // 0 or IPPROTO_xxx for IPv4 and IPv6
    --     size_t              ai_addrlen;     // Length of ai_addr
    --     char *              ai_canonname;   // Canonical name for nodename
    --     _Field_size_bytes_(ai_addrlen) struct sockaddr *   ai_addr;        // Binary address
    --     struct addrinfo *   ai_next;        // Next structure in linked list
    -- }
    type addrinfo is record
        ai_flags     : int                 := 0;
	    ai_family    : ai_flags_t          := 0;
	 	ai_socktype  : Socket_Type         := 0;
        ai_protocol  : int                 := 0;
        ai_addrlen   : Interfaces.C.size_t := 0;
        ai_canonname : chars_ptr           := Interfaces.C.Strings.Null_Ptr;
        ai_addr      : System.Address      := System.Null_Address;
        ai_next      : System.Address      := System.Null_Address;
    end record
        with Convention => C;

    --   C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\ws2def.h  
    -- #define AF_UNSPEC       0               // unspecified
    -- #define AF_UNIX         1               // local to host (pipes, portals)
    -- #define AF_INET         2               // internetwork: UDP, TCP, etc.
    AF_UNSPEC : constant ai_flags_t := 0;

    --   C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\ws2def.h  
    -- //
    -- // N.B. required for backwards compatability to support 0 = IP for the
    -- // level argument to get/setsockopt.
    -- //
    -- #define IPPROTO_IP              0
    -- //
    -- // Protocols.  The IPv6 defines are specified in RFC 2292.
    -- //
    -- typedef enum {
    -- #if(_WIN32_WINNT >= 0x0501)
    --     IPPROTO_HOPOPTS       = 0,  // IPv6 Hop-by-Hop options
    -- #endif//(_WIN32_WINNT >= 0x0501)
    --     IPPROTO_ICMP          = 1,
    IPPROTO_IP   : constant int := 0;
    IPPROTO_ICMP : constant int := 1;

    -- #define	AI_PASSIVE	0x00000001 /* get address to use bind() */
    -- #define	AI_CANONNAME	0x00000002 /* fill ai_canonname */
    -- #define	AI_NUMERICHOST	0x00000004 /* prevent host name resolution */
    -- #define	AI_NUMERICSERV	0x00001000 /* prevent service name resolution */
    AI_PASSIVE   : constant := 16#00000001#;
    AI_CANONNAME : constant := 16#00000002#;

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
    -- WINSOCK_API_LINKAGE
    -- INT
    -- WSAAPI
    -- getaddrinfo(
    --     _In_opt_        PCSTR               pNodeName,
    --     _In_opt_        PCSTR               pServiceName,
    --     _In_opt_        const ADDRINFOA *   pHints,
    --     _Outptr_        PADDRINFOA *        ppResult
    --     );
    function getaddrinfo(
        Target : Interfaces.C.Strings.chars_ptr;
        Unused : Interfaces.C.Strings.chars_ptr;
        Hints  : addrinfo_ptr;
        Result : System.Address
    ) return Interfaces.C.Int;
    pragma Import (Stdcall, getaddrinfo, "getaddrinfo");

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
    -- typedef UINT_PTR        SOCKET;
    subtype Socket_Descriptor is int;
    Invalid_Socket : constant Socket_Descriptor := -1;

    subtype Protocol_Family is int;

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
    -- WINSOCK_API_LINKAGE
    -- _Must_inspect_result_
    -- SOCKET
    -- WSAAPI
    -- socket(
    --     _In_ int af,
    --     _In_ int type,
    --     _In_ int protocol
    --     );
    --
    -- A -1 is returned if an error occurs, otherwise the return value is a
    -- descriptor referencing the socket.
    function socket(
        Communication_Domain    : Protocol_Family;
        Communication_Semantics : Socket_Type;
        Protocol                : int
    ) return Socket_Descriptor;        
    pragma Import (Stdcall, socket, "socket");

    subtype Connect_Status is int;
    Connect_Error   : constant Connect_Status := -1;
    Connect_Success : constant Connect_Status := 0;

    subtype Send_Status is ssize_t;
    Send_Error   : constant Send_Status := -1;
    Send_Success : constant Send_Status := 0;

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
    -- WINSOCK_API_LINKAGE
    -- int
    -- WSAAPI
    -- connect(
    --     _In_ SOCKET s,
    --     _In_reads_bytes_(namelen) const struct sockaddr FAR * name,
    --     _In_ int namelen
    --     );
    function connect (
        Socket         : Socket_Descriptor;
        Address        : System.Address;
        Address_Length : int
    ) return Connect_Status;
    pragma Import (Stdcall, connect, "connect");

    function close (File_Descriptor : int) return int;
    pragma Import (Stdcall, close, "close");

    -- Hide the return value of close() when we don't need it.
    procedure close (File_Descriptor : int);

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
    -- WINSOCK_API_LINKAGE
    -- int
    -- WSAAPI
    -- send(
    --     _In_ SOCKET s,
    --     _In_reads_bytes_(len) const char FAR * buf,
    --     _In_ int len,
    --     _In_ int flags
    --     );
    function send (
        Socket : Socket_Descriptor;
        Buffer : Void_Ptr;
        Length : Interfaces.C.size_t;
        Flags  : int
    ) return ssize_t;
    pragma Import (Stdcall, send, "send");

    -- #include <sys/socket.h>
    -- ssize_t
    -- recv(int socket, void *buffer, size_t length, int flags);
    function recv (
        Socket : Socket_Descriptor;
        Buffer : Void_Ptr;
        Length : Interfaces.C.size_t;
        Flags : int
    ) return ssize_t;
    pragma Import (Stdcall, recv, "recv");

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
    -- /*
    --  * Select uses arrays of SOCKETs.  These macros manipulate such
    --  * arrays.  FD_SETSIZE may be defined by the user before including
    --  * this file, but the default here should be >= 64.
    --  *
    --  * CAVEAT IMPLEMENTOR and USER: THESE MACROS AND TYPES MUST BE
    --  * INCLUDED IN WINSOCK2.H EXACTLY AS SHOWN HERE.
    --  */
    -- #ifndef FD_SETSIZE
    -- #define FD_SETSIZE      64
    -- #endif /* FD_SETSIZE */
    --
    -- typedef unsigned int    u_int;
    -- typedef struct fd_set {
    --         u_int fd_count;               /* how many are SET? */
    --         SOCKET  fd_array[FD_SETSIZE];   /* an array of SOCKETs */
    -- } fd_set;

    -- WINSOCK_API_LINKAGE
    -- int
    -- WSAAPI
    -- select(
    --     _In_ int nfds,
    --     _Inout_opt_ fd_set FAR * readfds,
    --     _Inout_opt_ fd_set FAR * writefds,
    --     _Inout_opt_ fd_set FAR * exceptfds,
    --     _In_opt_ const struct timeval FAR * timeout
    --     );
    --
    -- /*
    --  * Structure used in select() call, taken from the BSD file sys/time.h.
    --  */
    -- struct timeval {
    --         long    tv_sec;         /* seconds */
    --         long    tv_usec;        /* and microseconds */
    -- };
    --

    FD_SETSIZE : constant := 64;
    subtype u_int is Interfaces.C.unsigned;
    type Socket_Array is array (Positive range <>) of Socket_Descriptor;

    type fd_set is record
        fd_count : u_int;
        fd_array : Socket_Array (1 .. FD_SETSIZE);
    end record;

    subtype fd_set_ptr is System.Address;
    type timeval is record
        tv_sec  : Interfaces.C.long;
        tv_usec : Interfaces.C.long;
    end record;
    subtype timeval_ptr is System.Address;

    -- Can't call this "select" since that is a keyword.
    function selectsocket (Num_Sockets : Interfaces.C.int;
        Read_Sockets   : fd_set_ptr;
        Write_Sockets  : fd_set_ptr;
        Except_Sockets : fd_set_ptr;
        Timeout        : timeval_ptr
        ) return Socket_Descriptor;
    pragma Import (Stdcall, selectsocket, "Select");

    function Image (Self : addrinfo) return String;
    function Image (Ptr : Socket_Address_Conversions.Object_Pointer) return String;

end Networking.Sockets;
