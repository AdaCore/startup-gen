package body Utils is

   procedure Register_Memory_Map_Attributes
   is
      Error1 : constant String := Register_New_Attribute
                              ("Mem_Kind",
                              "Memory",
                              Indexed => True);

      Error2 : constant String := Register_New_Attribute
                           ("Size",
                           "Memory",
                           Indexed => True);

      Error3 : constant String := Register_New_Attribute
                           ("Start",
                           "Memory",
                           Indexed => True);

      Error4 : constant String := Register_New_Attribute
                           ("Memories",
                           "Memory",
                           Is_List => True);

   begin
      if Error1 /= "" or else Error2 /= "" or else Error3 /= ""
         or else Error4 /= ""
      then
         raise Program_Error with "Could not register new attribute.";
      end if;
   end Register_Memory_Map_Attributes;

end Utils;