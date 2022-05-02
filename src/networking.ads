with Interfaces.C.Strings;
with System;

-- src/networking.ads
package Networking is

    subtype chars_ptr is Interfaces.C.Strings.chars_ptr;
    subtype int is Interfaces.C.int;
    use type int;
    subtype Void_Ptr is System.Address;
    type ssize_t is mod 2 ** Interfaces.C.size_t'size;

    -- Convenience function for getting errno.
    function Get_Errno_String return String;

end Networking;
