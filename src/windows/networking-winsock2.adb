with Interfaces.C;
with System;

with Ada.Text_IO;

package body Networking.Winsock2 is

-- C:/Program Files (x86)/Windows Kits/10/Include/10.0.20348.0/shared/minwindef.h
-- typedef unsigned char   BYTE;
-- #define MAKEWORD(a, b)  ((WORD)(((BYTE)(((DWORD_PTR)(a)) & 0xff)) | ((WORD)((BYTE)(((DWORD_PTR)(b)) & 0xff))) << 8))
-- #define MAKEWORD(a, b)  ((WORD)(((BYTE)(((DWORD_PTR)(a)) & 0xff)) | ((WORD)((BYTE)(((DWORD_PTR)(b)) & 0xff))) << 8))
   function Make_Word (High, Low : WORD) return WORD is
      use type Interfaces.Unsigned_32;
   begin
      return WORD (Interfaces.Shift_Left (Interfaces.Unsigned_32 (High) and 16#FF#, 8)
            or (Interfaces.Unsigned_32 (Low) and 16#FF#));
   end Make_Word;

   pragma Assert (Make_Word (1, 2) = 258);
   pragma Assert (Make_Word (2, 2) = 514);

   Success : constant := 0;

   overriding
   procedure Initialize (Self : in out Singleton) is
      use type Interfaces.C.int;
   begin
      if WSAStartup (Make_Word (2, 2), Self.Data'Address) /= Success then
         raise Program_Error with "Unable to startup Winsock";
      end if;
      Ada.Text_IO.Put_Line ("Started up Winsock");      
   end Initialize;

   overriding
   procedure Finalize (Self : in out Singleton) is
      use type Interfaces.C.int;
   begin
      if Self.Initialized and WSACleanup /= Success then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Failed to shutdown winsock");
         Self.Initialized := False;
      end if;
   end Finalize;

end Networking.Winsock2;
