package body Utils is

   procedure Register_Memory_Map_Attributes
   is
      Error1 : constant String := Register_New_Attribute
                              ("Types",
                              "Memory",
                              Is_List => True);

      Error2 : constant String := Register_New_Attribute
                           ("Size",
                           "Memory",
                           Indexed => True);

      Error3 : constant String := Register_New_Attribute
                           ("Start",
                           "Memory",
                           Indexed => True);

   begin
      if Error1 /= "" or else Error2 /= "" or else Error3 /= "" then
         raise Program_Error;
      end if;
   end Register_Memory_Map_Attributes;

end Utils;
