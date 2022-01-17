with BB;            use BB;
with BB.ADC;        use BB.ADC;
with BB_PD_Control; use BB_PD_Control;
with ADC_Handler;   use ADC_Handler;
with ADC_Utils;     use ADC_Utils;
with CSV_Logs;      use CSV_Logs;
with IIR_Filters;   use IIR_Filters;

with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Real_Time;                use Ada.Real_Time;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

----------------------------
--  Control_With_Filters  --
----------------------------

procedure Control_With_Filters is
   procedure Filtered_Control_Loop
     (Log_File_Name   : String; Filter_Name : String;
      Filter_Function : access function
        (ADC_Sample : in ADC_Register) return Position;
      Kp : Float; Kd : Float; Period : Time_Span := Microseconds (50000))
   is
      Next       : Time;
      Target_Pos : Position := -100.0;

      ADC_Count            : ADC_Register;
      Filtered_Measurement : Position;
      
      Ang                  : Angle;
   begin
      Open_Log_Session (Log_File_Name);
      Log_Text ("Target_Pos, " & Filter_Name & ", Ang");

      Move_BB_To (Earth);
      Configure_PD (Kp, Kd, Target => Target_Pos);
      Attach_ADC_Handler(EOC_Handler'Access);
      
      Next := Clock;
      for I in 1 .. 200 loop
         delay until Next;

         -- Trigger conversion
         Set_False (End_Of_Conversion);
         Write_CR (Get_CR (Trigger => True, Interrupt_Enable => True));

         -- Wait for EOC Suspension Object
         Suspend_Until_True (End_Of_Conversion);

         -- Obtain first 12 MSBs
         ADC_Count := Read_DR and (2**12 - 1);

         -- Convert and filter raw measurement
         Filtered_Measurement := Filter_Function (ADC_Count);

         Calculate_Action (Filtered_Measurement, Ang);
         Set_Beam_Angle (Ang);

         Log_Data ((Target_Pos, Filtered_Measurement, Ang));

         if I = 100 then
            Target_Pos := 100.0;
            Set_Target_Position (Target_Pos);
         end if;

         Next := Next + Period;
      end loop;

      Close_Log_Session;
   end Filtered_Control_Loop;

begin
   Filtered_Control_Loop
     ("control_dummy.csv", "Dummy", Filter_Dummy'Access, Kp => 2.0,
      Kd                                                    => 35.0);
   Filtered_Control_Loop
     ("control_IIR80.csv", "IIR-80/20", Filter_80'Access, Kp => 0.06,
      Kd                                                     => 0.77);
   Filtered_Control_Loop
     ("control_IIR50.csv", "IIR-50/50", Filter_50'Access, Kp => 0.14,
      Kd                                                     => 0.95);
end Control_With_Filters;
