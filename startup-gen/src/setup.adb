with Ada.Text_IO;                            use Ada.Text_IO;
with Ada.Containers.Indefinite_Ordered_Maps;
with GNAT.Command_Line;                      use GNAT.Command_Line;
with Utils;

package body Setup is

   package Scv_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => String);
   Scv_Map : Scv_Maps.Map;
   --  All defined scenario variables

   procedure Add_Scenario_Var (Key, Value : String);
   procedure Handle_Scenerio_Arg (Switch, Value : String);

   ----------------------
   -- Add_Scenario_Var --
   ----------------------

   procedure Add_Scenario_Var (Key, Value : String) is
   begin
      Scv_Map.Include (Key, Value);
   end Add_Scenario_Var;

   -------------------------
   -- Handle_Scenerio_Arg --
   -------------------------

   procedure Handle_Scenerio_Arg (Switch, Value : String) is
      pragma Unreferenced (Switch);
      Name_First, Name_Last, Value_First : Natural;
   begin
      Name_First := Value'First;
      Name_Last := Name_First - 1;
      while Name_Last < Value'Last
        and then Value (Name_Last + 1) /= '='
      loop
         Name_Last := Name_Last + 1;
      end loop;

      Value_First := Name_Last + 2;

      Add_Scenario_Var
        (Key   => Value (Name_First .. Name_Last),
         Value => Value (Value_First .. Value'Last));
   end Handle_Scenerio_Arg;

   -------------------
   -- Get_Arguments --
   -------------------

   procedure Get_Arguments (Values : aliased out Command_Line_Values)

   is
      Config : Command_Line_Configuration;

   begin
      Define_Switch
         (Config,
          Values.Linker_File'Access,
          "-l:",
          Help => "Name of the generated linker script.");

      Define_Switch
         (Config,
          Values.Startup_Code_File'Access,
          "-s:",
          Help => "Name of the generated startup code.");

      Define_Switch
         (Config,
          Handle_Scenerio_Arg'Access,
          "-X:",
          Help => "Specify an external reference for Project Files.");

      Define_Switch
         (Config,
          Values.Project_File'Access,
          "-P:",
          Help => "Name of the project file with the device configuation.");

      Getopt (Config);

      if Values.Project_File = null or else Values.Project_File.all = "" then
         Utils.Fatal_Error ("Project file required (-P)");
      end if;
   end Get_Arguments;

   ------------------------------
   -- Apply_Scenario_Variables --
   ------------------------------

   procedure Apply_Scenario_Variables (Env : Project_Environment_Access) is
      use Scv_Maps;
   begin
      for Scv_C in Scv_Map.Iterate loop
         Change_Environment (Env.all, Key (Scv_C), Element (Scv_C));
      end loop;
   end Apply_Scenario_Variables;

   -------------
   -- Display --
   -------------

   procedure Display (Values : in out Command_Line_Values)
   is
   begin
      Put_Line ("Spec " & Values.Project_File.all);
      Put_Line ("Linker Script " & Values.Linker_File.all);
      Put_Line ("Startup Code " & Values.Startup_Code_File.all);
   end Display;

end Setup;