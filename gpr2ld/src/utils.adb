package body Utils is

   procedure Register_Memory_Map_Attributes
   is
      Error1 : aliased constant String := Register_New_Attribute
                              ("Mem_Kind",
                              "Memory",
                              Indexed => True);

      Error2 : aliased constant String := Register_New_Attribute
                           ("Size",
                           "Memory",
                           Indexed => True);

      Error3 : aliased constant String := Register_New_Attribute
                           ("Start",
                           "Memory",
                           Indexed => True);

      Error4 : aliased constant String := Register_New_Attribute
                           ("Memories",
                           "Memory",
                           Is_List => True);

      Error5 : aliased constant String := Register_New_Attribute
                           ("Name", "CPU");

      Error6 : aliased constant String := Register_New_Attribute
                           ("Float_Handling", "CPU");

      type Err_Str_Access is access constant String;
      type Strings is array (Integer range <>) of Err_Str_Access;
      Errors : constant Strings :=
         (1 => Error1'Access,
          2 => Error2'Access,
          3 => Error3'Access,
          4 => Error4'Access,
          5 => Error5'Access,
          6 => Error6'Access
         );
   begin
      for Str of Errors loop
         if Str.all /= "" then
            raise Program_Error with "Could not register new attribute.";
         end if;
      end loop;
   end Register_Memory_Map_Attributes;

end Utils;
