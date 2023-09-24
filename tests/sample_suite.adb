with AUnit.Test_Suites; use AUnit.Test_Suites;

--  List of tests and suites to compose:
with Test_XML;
function Sample_Suite return Access_Test_Suite is
   Result : Access_Test_Suite := new Test_Suite;
begin
   --  You may add multiple tests or suites here:
   Add_Test (Result, new Test_XML.Test_Case);
   return Result;
end Sample_Suite;


