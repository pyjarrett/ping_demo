with Interfaces.C;
with System.Address_To_Access_Conversions;

-- The basic networking types to make the rest of the code go.
package Networking.Types is
   use type Interfaces.C.int;
   use type Interfaces.Unsigned_64;

   subtype Connect_Status is int;

   Connect_Error   : constant Connect_Status := -1;
   Connect_Success : constant Connect_Status := 0;

   subtype Send_Status is int;
   Send_Error   : constant Send_Status := -1;
   Send_Success : constant Send_Status := 0;

   -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\basetsd.h
   --  typedef unsigned __int64 UINT_PTR, *PUINT_PTR;
   -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
   -- typedef UINT_PTR        SOCKET;
   subtype Socket_Descriptor is Interfaces.Unsigned_64;
   Invalid_Socket : constant Socket_Descriptor := -1;

   type Socket_Protocol is new int;
   type Protocol_Family is new int;
   type Socket_Type is new int;

   -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\ws2def.h
   -- //
   -- // Socket types.
   -- //
   -- #define SOCK_STREAM     1
   -- #define SOCK_DGRAM      2
   -- #define SOCK_RAW        3
   -- #define SOCK_RDM        4
   -- #define SOCK_SEQPACKET  5    
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

   subtype socklen_t is Interfaces.C.size_t;

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
      ai_flags     : ai_flags_t          := 0;
      ai_family    : Protocol_Family     := 0;
      ai_socktype  : Socket_Type         := 0;
      ai_protocol  : Socket_Protocol     := 0;
      ai_addrlen   : socklen_t           := 0;
      ai_canonname : chars_ptr           := Interfaces.C.Strings.Null_Ptr;
      ai_addr      : System.Address      := System.Null_Address;
      ai_next      : System.Address      := System.Null_Address;
   end record
      with Convention => C;

   --   C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\ws2def.h  
   -- #define AF_UNSPEC       0               // unspecified
   -- #define AF_UNIX         1               // local to host (pipes, portals)
   -- #define AF_INET         2               // internetwork: UDP, TCP, etc.
   AF_UNSPEC : constant Protocol_Family := 0;

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
   IPPROTO_IP   : constant Socket_Protocol := 0;
   IPPROTO_ICMP : constant Socket_Protocol := 1;

   -- #define	AI_PASSIVE	0x00000001 /* get address to use bind() */
   -- #define	AI_CANONNAME	0x00000002 /* fill ai_canonname */
   -- #define	AI_NUMERICHOST	0x00000004 /* prevent host name resolution */
   -- #define	AI_NUMERICSERV	0x00001000 /* prevent service name resolution */
   AI_PASSIVE   : constant ai_flags_t := 16#00000001#;
   AI_CANONNAME : constant ai_flags_t := 16#00000002#;

   -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
   -- #define POLLRDNORM  0x0100
   -- #define POLLRDBAND  0x0200
   -- #define POLLIN      (POLLRDNORM | POLLRDBAND)
   -- #define POLLPRI     0x0400

   -- #define POLLWRNORM  0x0010
   -- #define POLLOUT     (POLLWRNORM)
   -- #define POLLWRBAND  0x0020

   -- #define POLLERR     0x0001
   -- #define POLLHUP     0x0002
   -- #define POLLNVAL    0x0004
   type Poll_Events is mod 2 ** Interfaces.C.short'Size;

   POLLRDNORM  : constant Poll_Events := 16#0100#;
   POLLRDBAND  : constant Poll_Events := 16#0200#;
   POLLIN      : constant Poll_Events := (POLLRDNORM or POLLRDBAND);
   POLLPRI     : constant Poll_Events := 16#0400#;

   POLLWRNORM  : constant Poll_Events := 16#0010#;
   POLLOUT     : constant Poll_Events := (POLLWRNORM);
   POLLWRBAND  : constant Poll_Events := 16#0020#;

   POLLERR     : constant Poll_Events := 16#0001#;
   POLLHUP     : constant Poll_Events := 16#0002#;
   POLLNVAL    : constant Poll_Events := 16#0004#;
   
end Networking.Types;