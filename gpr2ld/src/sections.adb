--------------
-- Sections --
--------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Sections is

   ------------------
   -- Make_Section --
   ------------------

   function Make_Section
      (Boot_Memory  : Unbounded_String;
       Name         : Unbounded_String;
       Reloc_Memory : Unbounded_String;
       Init_Code    : Algorithm := No_Code;
       Force_Init   : Boolean := False)
       return Section
   is
      To_Init : constant Boolean := Force_Init or else
                                       (Boot_Memory /= Reloc_Memory);
      Lowered_Name : constant Unbounded_String :=
         To_Unbounded_String (To_Lower (To_String (Name)));
      Temp : Section :=
         (Name               => Lowered_Name,
          Reloc_Memory       => Reloc_Memory,
          To_Init            => To_Init,
          Init_Code          => Init_Code,
          Additional_Content => Unbounded_String_Vectors.Empty_Vector);
   begin
      Temp.Init_Code.Format_Code_With_Name (Temp.Name);
     return Temp;
   end Make_Section;

end Sections;
