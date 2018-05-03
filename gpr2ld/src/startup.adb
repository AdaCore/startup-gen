
with Ada.Text_IO; use Ada.Text_IO;
package body Startup is

   ------------
   -- Format --
   ------------

   procedure Format
      (Self   : in out Algorithm;
       Name   : Unbounded_String;
       Indent : Unbounded_String)
   is
   begin
      Self.Format_Code_With_Name (Name);
      Self.Format_Code_With_Indent (Indent);
   end Format;

   -----------------
   -- Format_Code --
   -----------------

   procedure Format_Code
      (Self : in out Algorithm;
       Subst : Substitution_Value)
   is
      Substitutions : constant Substitution_Array := (1 => Subst);
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

   ---------------
   -- Get_Lines --
   ---------------

   function Get_Lines (Self : in out Algorithm)
      return Unbounded_String_Vectors.Vector
   is
   begin
      return Self.Code;
   end Get_Lines;

   ----------------------
   -- Get_File_Content --
   ----------------------
   function Get_File_Content (File : Virtual_File) return Algorithm
   is
      Line   : Unbounded_String := To_Unbounded_String ("");
      Vector : Unbounded_String_Vectors.Vector :=
         Unbounded_String_Vectors.Empty_Vector;
      EOL    : constant Unbounded_String := To_Unbounded_String ("" & ASCII.LF);
      Temp   : Algorithm;
   begin
      for Char of Read_File (File) loop
         declare
            UStr : constant Unbounded_String :=
               To_Unbounded_String ("" & Char);
         begin
            if UStr = EOL then
               Vector := Vector & Line;
               Line := To_Unbounded_String ("");
            else
               Line := Line & UStr;
            end if;
         end;
      end loop;
      Temp.Code := Vector;
      return Temp;
   end Get_File_Content;

   -------------
   -- Display --
   -------------

   procedure Display (Self : in out Algorithm) is
   begin
      for Line of Self.Code loop
         Put_Line ( To_String (Line));
      end loop;
   end Display;

end Startup;
