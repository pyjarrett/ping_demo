with Interfaces.C.Strings;
with System;

-- GNAT-specific function
with System.Address_Image;

with System.Storage_Elements;

-- src/networking.ads

-- Top level package for dealing with networking.
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

    -- Convenience function for printing an address.
    function Image (Address : System.Address) return String is (System.Address_Image (Address));
    
    function Calculate_Checksum (Buffer : System.Storage_Elements.Storage_Array)
        return U16
        with Inline;
    
    function Calculate_Checksum (Buffer : U8_Buffer)
        return U16;
    
    type Unsigned_2 is mod 2 ** 2;
    type Unsigned_3 is mod 2 ** 3;
    type Unsigned_4 is mod 2 ** 4;
    type Unsigned_6 is mod 2 ** 6;
    type Unsigned_13 is mod 2 ** 13;
    type IPv4_Header is record
        Version                : Unsigned_4;
        Internet_Header_Length : Unsigned_4;
        DSCP                   : Unsigned_6;
        ECN                    : Unsigned_2;
        Total_Length           : Interfaces.Unsigned_16;
        Identification         : Interfaces.Unsigned_16;
        Flags                  : Unsigned_3;
        Fragment_Offset        : Unsigned_13;
        Time_To_Live           : Interfaces.Unsigned_8;
        Protocol               : Interfaces.Unsigned_8;
        Header_Checksum        : Interfaces.Unsigned_16;
        Source_IP              : Interfaces.Unsigned_32;
        Destination_IP         : Interfaces.Unsigned_32;
    end record;

    for IPv4_Header use record
        Version                at 0 range 0  .. 3;
        Internet_Header_Length at 0 range 4  .. 7;
        DSCP                   at 0 range 8  .. 13;
        ECN                    at 0 range 14 .. 15;
        Total_Length           at 0 range 16 .. 31;
        Identification         at 4 range 0  .. 15;
        Flags                  at 4 range 16 .. 18;
        Fragment_Offset        at 4 range 19 .. 31;
        Time_To_Live           at 8 range 0  .. 7;
        Protocol               at 8 range 8  .. 15;
        Header_Checksum        at 8 range 16 .. 31;
        Source_IP              at 12 range 0 .. 31;
        Destination_IP         at 16 range 0 .. 31;
    end record;
    for IPv4_Header'Bit_Order use System.High_Order_First;
    for IPv4_Header'Scalar_Storage_Order use System.High_Order_First;

    procedure Print_Bytes (Address : System.Address; Num_Bytes : Natural);
        
end Networking;
