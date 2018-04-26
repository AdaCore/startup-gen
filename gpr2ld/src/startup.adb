package body Startup is

   -----------------
   -- Format_Code --
   -----------------

   procedure Format_Code
      (Self : in out Algorithm;
       Subst : Substitution_Value)
   is
      Substitutions : constant Substitution_Array :=
         (1 => Subst);
   begin
      for Line of Self.Code loop
         declare
            Substituted_Code : constant String :=
               Substitute (Str        => To_String (Line),
                           Substrings => Substitutions,
                           Delimiter  => '$');
         begin
            Line := To_Unbounded_String (Substituted_Code);
         end;
      end loop;
   end Format_Code;

   ---------------------------
   -- Format_Code_With_Name --
   ---------------------------

  procedure Format_Code_With_Name
      (Self : in out Algorithm;
       Name : Unbounded_String)
       is
       begin
         Self.Format_Code
            (Subst => (Name  => new String'("NAME"),
                       Value => new String'(To_String (Name))));
       end Format_Code_With_Name;

   -----------------------------
   -- Format_Code_With_Indent --
   -----------------------------

   procedure Format_Code_With_Indent
      (Self : in out Algorithm;
       Indent : Unbounded_String)
       is
       begin
         Self.Format_Code
            (Subst => (Name  => new String'("INDENT"),
                       Value => new String'(To_String (Indent))));
       end Format_Code_With_Indent;

   -----------------------
   -- Clear_Memory_Code --
   -----------------------

   function Clear_Memory_Code return Algorithm is
      Clear_Memory : constant Algorithm :=
         (Load => False,
          Code => SV.Empty_Vector &
               TUS ("/* Clear $NAME */") &
               TUS ("$INDENTmovw  r0,#:lower16:__$NAME_start") &
               TUS ("$INDENTmovw  r0,#:uppper16:__$NAME_start") &
               TUS ("$INDENTmovw  r1,#:lower16:__$NAME_words") &
               TUS ("$INDENTmovw  r2,#0") &
               TUS ("$INDENTmovw  r1,1f") &
               TUS ("$INDENTcbz  r1,1f") &
               TUS ("0:$INDENTstr  r2,[r0],#4") &
               TUS ("$INDENTsubs  r1,r1,#1") &
               TUS ("$INDENTbne  0b") &
               TUS ("") &
               TUS ("1:")
            );
   begin
      return Clear_Memory;
   end Clear_Memory_Code;

   ----------------------
   -- Copy_Memory_Code --
   ----------------------

   function Copy_Memory_Code return Algorithm is
      Copy_Memory : constant Algorithm :=
         (Load => True,
          Code => SV.Empty_Vector &
               TUS ("/* Copy $NAME */") &
               TUS ("$INDENTmovw  r0,#:lower16:__$NAME_start") &
               TUS ("$INDENTmovw  r0,#:uppper16:__$NAME_start") &
               TUS ("$INDENTmovw  r1,#:lower16:__$NAME_words") &
               TUS ("$INDENTmovw  r2,#:lower16:__$NAME_load") &
               TUS ("$INDENTmovw  r2,#:uppper16:__$NAME_load") &
               TUS ("$INDENTcbz  r1,1f") &
               TUS ("0:$INDENTldr  r4,[r2],#4") &
               TUS ("$INDENTstr  r4,[r0],#4") &
               TUS ("$INDENTsubs  r1,r1,#1") &
               TUS ("$INDENTbne  0b") &
               TUS ("") &
               TUS ("1:")
            );
   begin
      return Copy_Memory;
   end Copy_Memory_Code;

   -------------
   -- No_Code --
   -------------

   function No_Code return Algorithm is
   begin
      return (Load => True, Code => SV.Empty_Vector);
   end No_Code;

end Startup;
