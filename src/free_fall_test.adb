with BB;       use BB;
with BB.Ideal; use BB.Ideal;
with BB.GUI;

procedure Free_Fall_Test is

begin

   Set_Simulation_Mode (Open_Loop);

   for Location in Solar_System_Object loop

      Move_BB_To (Location);

      for I in 1 .. 2 loop

         Set_Beam_Angle (2.0);
         delay 2.5;

         Set_Beam_Angle (-2.0);
         delay 2.5;

      end loop;

      delay 2.0;

   end loop;

end Free_Fall_Test;
