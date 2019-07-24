package Utils is

   Prj_Package_Name : constant String := "Device_Configuration";

   procedure Register_Memory_Map_Attributes;

   Exit_Exc : exception;

   procedure Warning (Msg : String);
   procedure Error (Msg : String);
   procedure Fatal_Error (Msg : String)
     with No_Return;

end Utils;
