with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with GNATCOLL.Templates; use GNATCOLL.Templates;
with GNATCOLL.VFS; use GNATCOLL.VFS;

-------------
-- Startup --
-------------

package Startup is

   type Algorithm is tagged private;

   No_Code           : constant Algorithm;

   package Unbounded_String_Vectors is new Ada.Containers.Vectors
      (Positive, Unbounded_String);

   function Get_Lines (Self : in out Algorithm)
      return Unbounded_String_Vectors.Vector;

   procedure Format
      (Self   : in out Algorithm;
       Name   : Unbounded_String;
       Indent : Unbounded_String);

   function Get_File_Content (File : Virtual_File) return Algorithm;

   procedure Display (Self : in out Algorithm);

private

   procedure Format_Code
      (Self : in out Algorithm;
       Subst : Substitution_Value);

   procedure Format_Code_With_Name
      (Self : in out Algorithm;
       Name : Unbounded_String);

   procedure Format_Code_With_Indent
      (Self : in out Algorithm;
       Indent : Unbounded_String);

   use Unbounded_String_Vectors;

   package SV renames Unbounded_String_Vectors;

   type Algorithm is tagged record
      Code : Unbounded_String_Vectors.Vector;
   end record;

   function TUS (Str : String) return Unbounded_String
      renames To_Unbounded_String;

   --  Private constants  --
   No_Code : constant Algorithm := (Code => SV.Empty_Vector);

end Startup;
