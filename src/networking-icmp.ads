with Ada.Unchecked_Conversion;

package Networking.ICMP is
    
    procedure Ping (Host : String; Payload : String);

end Networking.ICMP;