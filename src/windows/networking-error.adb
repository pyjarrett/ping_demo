with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body Networking.Error is

   function Get_Errno_String return String
   is
      -- errno.h
      -- #if _CRT_FUNCTIONS_REQUIRED
      --     _ACRTIMP int* __cdecl _errno(void);
      --     #define errno (*_errno())
      function errno return System.Address
         with Import, Convention => C, External_Name => "_errno";

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

end Networking.Error;
