data PistonEngineConfiguration =
  Parallel
  | VConfiguration
  | WConfiguration
  | XConfiguration
  | Opposed
  | Radial

data PistonEngineCycle =
  FourStroke
  | TwoStroke

data PistonEngine =
  PistonEngineConfiguration
  PistonEngineCycle
  Positive -- cylinders
  Positive -- capacity

data RotaryEngine =
  Positive -- rotors
  Positive -- capacity

data InternalCombustionEngineAirInduction =
  Supercharged
  | Turbocharged
  | SuperTurbocharged
  | NaturalInduction

data InternalCombustionEngineFuelInduction =
  Carburetor
  | FuelInjection

data InternalCombustionEngineIgnition =
  Diesel
  | Spark

data InternalCombustionEngineType =
  _ PistonEngine
  | _ RotaryEngine
 
data InternalCombustionEngine =
  InternalCombustionEngineAirInduction
  InternalCombustionEngineFuelInduction
  InternalCombustionEngineIgnition
  InternalCombustionEngineType

data JetType =
  Turbojet
  | Turbofan
  | Turboprop
  | Ramjet
  | Scramjet

data ElectricType =
  BrushlessAC
  | BrushedAC
  | Brushless DC
  | BrushedDC
  | UniversalACDC
  | SwitchedReluctance
  | DirectDrive
  | Linear

data PropellorControl =
  FixedPitch
  | ConstantSpeedUnit

data EngineType =
  _ InternalCombustionEngine PropellorControl
  | _ ElectricType PropellorControl
  | _ JetType
  | Rocket

data Engine =
  String -- manufacturer
  String -- designation
  EngineType
