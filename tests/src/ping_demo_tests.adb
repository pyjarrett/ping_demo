with Trendy_Test.Reports;

with Networking.Tests;

procedure Ping_Demo_Tests is
begin
   Trendy_Test.Register(Networking.Tests.All_Tests);
   Trendy_Test.Reports.Print_Basic_Report (Trendy_Test.Run);
end Ping_Demo_Tests;
