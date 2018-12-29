data Event =
  Simulator
  Medical
  Flight
    Towing
    Towed
    FlightExam
    BFR
    FlightCheck
    CrossCountry
    Formation
    Aerobatic
    Miscellaneous
      AsPAX
    FlightRules
      VFR
      IFR
  WrittenExam
  EnglishProficiency
  Briefing
  [Expense]
     
data Aviator =
  Aviator
    [PublicKey]
    PasswordHash

data Media =
  Image
  Video
  Audio
  TrackLog
  Link
 
data Expense =

data ViewAuthorisation =
  AviatorsAuthorised (NonEmpty Aviator)
  UsingPasswordHash
  Anyone
 
