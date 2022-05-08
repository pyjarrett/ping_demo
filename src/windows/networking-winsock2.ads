with Ada.Finalization;

-- Winsock2 requires additional setup and teardown that Mac and Linux sockets
-- do not require.
package Networking.Winsock2 is

	-- Type to track startup and shutdown.  The type is only needed because
	-- there needs to be teardown code called on program shutdown.
	type Singleton is limited private;

private

	-- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\minwindef.h
	-- typedef unsigned short      WORD;
	subtype short is Interfaces.C.short;
	subtype WORD is Interfaces.C.unsigned_short;
	subtype char_array is Interfaces.C.char_array;

	subtype far_char_ptr is System.Address;

	use type Interfaces.C.size_t;
	use type Interfaces.C.unsigned_short;

	WSADESCRIPTION_LEN : constant := 256;
	WSASYS_STATUS_LEN  : constant := 128;

	-- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
	-- #define WSADESCRIPTION_LEN      256
	-- #define WSASYS_STATUS_LEN       128
	--
	-- typedef struct WSAData {
	--         WORD                    wVersion;
	--         WORD                    wHighVersion;
	-- #ifdef _WIN64
	--         unsigned short          iMaxSockets;
	--         unsigned short          iMaxUdpDg;
	--         char FAR *              lpVendorInfo;
	--         char                    szDescription[WSADESCRIPTION_LEN+1];
	--         char                    szSystemStatus[WSASYS_STATUS_LEN+1];
	-- #else
	--         char                    szDescription[WSADESCRIPTION_LEN+1];
	--         char                    szSystemStatus[WSASYS_STATUS_LEN+1];
	--         unsigned short          iMaxSockets;
	--         unsigned short          iMaxUdpDg;
	--         char FAR *              lpVendorInfo;
	-- #endif
	-- } WSADATA, FAR * LPWSADATA;
	--
	--
	-- #define FAR                 far
	-- #define far
	type WSAData is record
		wVersion       : WORD;
		wHighVersion   : WORD;
		iMaxSockets    : short;
		iMaxUdpDg      : short;
		lpVendorInfo   : far_char_ptr;
		szDescription  : char_array (1 .. WSADESCRIPTION_LEN + 1);
		szSystemStatus : char_array (1 .. WSASYS_STATUS_LEN + 1);
	end record
		with Convention => C;

	type Singleton is new Ada.Finalization.Limited_Controlled with record
		Data        : WSAData;
		Initialized : Boolean := False;
	end record;

	overriding procedure Initialize (Self : in out Singleton);
   overriding procedure Finalize   (Self : in out Singleton);

	-- /*
	--  * Establish DLL function linkage if supported by the current build
	--  * environment and not previously defined.
	--  */
	-- #ifndef WINSOCK_API_LINKAGE
	-- #ifdef DECLSPEC_IMPORT
	-- #define WINSOCK_API_LINKAGE DECLSPEC_IMPORT
	-- #else
	-- #define WINSOCK_API_LINKAGE
	-- #endif
	-- #endif
	--
	-- #define _Must_inspect_result_    _SAL2_Source_(_Must_inspect_result_, (), _Must_inspect_impl_ _Check_return_impl_)
	--    
	-- #define PASCAL      __stdcall
	-- #define WSAAPI                  FAR PASCAL
	--
	-- #if INCL_WINSOCK_API_PROTOTYPES
	-- WINSOCK_API_LINKAGE
	-- _Must_inspect_result_
	-- int
	-- WSAAPI
	-- WSAStartup(
	--     _In_ WORD wVersionRequested,
	--     _Out_ LPWSADATA lpWSAData
	--     );
	-- #endif /* INCL_WINSOCK_API_PROTOTYPES */
	function WSAStartup (wVersionRequested : WORD; lpWSAData : System.Address) return Interfaces.C.int
			with Import, Convention => Stdcall, External_Name => "WSAStartup";
	
	-- WINSOCK_API_LINKAGE
	-- int
	-- WSAAPI
	-- WSACleanup(
	--     void
	--     );
	function WSACleanup return Interfaces.C.int
		with Import, Convention => Stdcall, External_Name => "WSACleanup";

	function Make_Word (High, Low : WORD) return WORD;

end Networking.Winsock2;
