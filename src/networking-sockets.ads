with Interfaces.C;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package Networking.Sockets is
    use type Interfaces.C.int;

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

    subtype addrinfo_ptr is System.Storage_Elements.Integer_Address;
    type ai_flags_t is mod 2 ** int'Size;

    -- /usr/include/sys/_types/_socklen_t.h
    -- typedef __darwin_socklen_t      socklen_t;
    -- Mac
    -- subtype socklen_t is int;

    subtype socklen_t is Interfaces.C.size_t;

    -- /usr/include/arm/_types.h
    -- typedef __uint32_t              __darwin_socklen_t;     /* socklen_t (duh) */

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
    -- type addrinfo is record
    --     ai_flags     : int          := 0;
	--     ai_family    : ai_flags_t   := 0;
	--  	ai_socktype  : Socket_Type  := 0;
    --     ai_protocol  : int          := 0;
    --     ai_addrlen   : socklen_t    := 0;
    --     ai_canonname : chars_ptr    := Interfaces.C.Strings.Null_Ptr;
    --     ai_addr      : System.Address := System.Null_Address;
    --     ai_next      : System.Address := System.Null_Address;
    -- end record
    --     with Convention => C;


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
    ) return Interfaces.C.Int;
        -- with Import, Convention => C;

    pragma Import (Stdcall, getaddrinfo, "getaddrinfo");

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
    ) return Socket_Descriptor;
        -- with Import, Convention => C;
        
    pragma Import (Stdcall, socket, "socket");

    subtype Connect_Status is int;
    Connect_Error   : constant Connect_Status := -1;
    Connect_Success : constant Connect_Status := 0;

    subtype Send_Status is ssize_t;
    Send_Error   : constant Send_Status := -1;
    Send_Success : constant Send_Status := 0;

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
    procedure close (File_Descriptor : int);

    -- #include <sys/socket.h>
    -- ssize_t
    -- send(int socket, const void *buffer, size_t length, int flags);
    function send (
        Socket : Socket_Descriptor;
        Buffer : Void_Ptr;
        Length : Interfaces.C.size_t;
        Flags  : int
    ) return ssize_t
        with Import, Convention => C;


    -- #include <sys/socket.h>
    -- ssize_t
    -- recv(int socket, void *buffer, size_t length, int flags);
    function recv (
        Socket : Socket_Descriptor;
        Buffer : Void_Ptr;
        Length : Interfaces.C.size_t;
        Flags : int
    ) return ssize_t
        with Import, Convention => C;

    function Image (Self : addrinfo) return String;
    function Image (Ptr : Socket_Address_Conversions.Object_Pointer) return String;

end Networking.Sockets;
