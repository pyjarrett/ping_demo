with Interfaces.C;

with Networking.Types;  use Networking.Types;

-- Low-level bindings to Windows socket routines.
--
-- Winsock shares much functionality with socket libraries on Linux and Mac,
-- with the added flare of added flare of an initialization and shutdown
-- routine.
--
-- A huge problem is that the Winsock functions use stdcall calling conventions
-- while the Mac and Linux versions just use cdecl.
package Networking.Sockets is
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
    ) return Interfaces.C.int
        with Import, Convention => Stdcall, External_Name => "getaddrinfo";

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
        Protocol                : Socket_Protocol
    ) return Socket_Descriptor
        with Import, Convention => Stdcall, External_Name => "socket";

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
    ) return Connect_Status
        with Import, Convention => Stdcall, External_Name => "connect";

    function close (File_Descriptor : int) return int
        with Import, Convention => Stdcall, External_Name => "close";

    -- Hide the return value of close() when we don't need it.
    procedure close (File_Descriptor : Socket_Descriptor);

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
    ) return ssize_t
        with Import, Convention => Stdcall, External_Name => "send";

    -- #include <sys/socket.h>
    -- ssize_t
    -- recv(int socket, void *buffer, size_t length, int flags);
    function recv (
        Socket : Socket_Descriptor;
        Buffer : Void_Ptr;
        Length : Interfaces.C.size_t;
        Flags : int
    ) return ssize_t
        with Import, Convention => Stdcall, External_Name => "recv";

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
    ) return Socket_Descriptor
        with Import, Convention => Stdcall, External_Name => "select";

    function Image (Self : addrinfo) return String;
    function Image (Ptr : Socket_Address_Conversions.Object_Pointer) return String;

end Networking.Sockets;
