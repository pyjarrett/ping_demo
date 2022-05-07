with Interfaces.C.Strings;
with System;

-- GNAT-specific function
with System.Address_Image;

with System.Storage_Elements;

-- src/networking.ads

-- Child packages can use these functions without using a prefix.
package Networking is

    subtype chars_ptr is Interfaces.C.Strings.chars_ptr;
    subtype int is Interfaces.C.int;
    subtype Void_Ptr is System.Address;
    type ssize_t is mod 2 ** Interfaces.C.size_t'size;

    subtype U8 is Interfaces.Unsigned_8;
    subtype U16 is Interfaces.Unsigned_16;
    subtype U32 is Interfaces.Unsigned_32;
    subtype U64 is Interfaces.Unsigned_64;

    type U8_Buffer is array (Positive range <>) of U8;
    type U16_Buffer is array (Positive range <>) of U16;

    -- Convenience function for getting errno.
    function Get_Errno_String return String;

    -- Convenience function for printing an address.
    function Image (Address : System.Address) return String is (System.Address_Image (Address));
    
    function Calculate_Checksum (Buffer : System.Storage_Elements.Storage_Array)
        return U16
        with Inline;
    
    function Calculate_Checksum (Buffer : U8_Buffer)
        return U16;
        
end Networking;
