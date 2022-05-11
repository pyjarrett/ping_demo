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
        Address_Length : socklen_t
    ) return Connect_Status
        with Import, Convention => Stdcall, External_Name => "connect";

    function close (File_Descriptor : Socket_Descriptor) return int
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

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
    -- #if INCL_WINSOCK_API_PROTOTYPES
    -- WINSOCK_API_LINKAGE
    -- int
    -- WSAAPI
    -- recv(
    --     _In_ SOCKET s,
    --     _Out_writes_bytes_to_(len, return) __out_data_source(NETWORK) char FAR * buf,
    --     _In_ int len,
    --     _In_ int flags
    --     );
    -- #endif /* INCL_WINSOCK_API_PROTOTYPES */
    function recv (
        Socket : Socket_Descriptor;
        Buffer : Void_Ptr;
        Length : Interfaces.C.size_t;
        Flags : int
    ) return int
        with Import, Convention => Stdcall, External_Name => "recv";

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
    -- typedef struct pollfd {
    --     SOCKET  fd;
    --     SHORT   events;
    --     SHORT   revents;
    -- } WSAPOLLFD, *PWSAPOLLFD, FAR *LPWSAPOLLFD;
    type pollfd is record
        fd      : Socket_Descriptor;
        events  : Poll_Events;
        revents : Poll_Events;
    end record
        with Convention => C;
    type pollfd_array is array (Positive range <>) of pollfd with Convention => C;

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\minwindef.h
    -- typedef unsigned long ULONG;
    -- typedef int                 INT;

    -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
    -- #if(_WIN32_WINNT >= 0x0600)
    -- #if INCL_WINSOCK_API_PROTOTYPES
    -- WINSOCK_API_LINKAGE
    -- int
    -- WSAAPI
    -- WSAPoll(
    --     _Inout_ LPWSAPOLLFD fdArray,
    --     _In_ ULONG fds,
    --     _In_ INT timeout
    --     );
    -- #endif /* INCL_WINSOCK_API_PROTOTYPES */
    -- #endif // (_WIN32_WINNT >= 0x0600)
    type Num_FDs is new Interfaces.C.unsigned_long;

    function poll (
        fd_array             : in out pollfd_array;
        fds                  : Num_FDs;
        Timeout_Milliseconds : Interfaces.C.int
    ) return Interfaces.C.int
        with Import, Convention => Stdcall, External_Name => "WSAPoll";

    function Image (Self : addrinfo) return String;
    function Image (Ptr : Socket_Address_Conversions.Object_Pointer) return String;

end Networking.Sockets;
