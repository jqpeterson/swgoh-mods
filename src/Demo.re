type shape = Square | Arrow | Diamond | Triangle | Circle | Cross;

type statset = CriticalChanceSet | CriticalDamageSet | DefenseSet | HealthSet | OffenseSet | PotencySet | SpeedSet | TenacitySet;

type pips = Pip1 | Pip2 | Pip3 | Pip4 | Pip5;

type tier = Grey | Green | Blue | Purple | Gold;

type primarystat = PrimaryAccuracy | PrimaryCriticalAvoidance | PrimaryCriticalChance | PrimaryCriticalDamage | PrimaryDefense | PrimaryHealth | PrimaryOffense | PrimaryPotency | PrimaryProtection | PrimarySpeed | PrimaryTenacity;

type primarylevel = PrimaryLevel (int);

type primaryvalue = PrimaryFloat (float)

type secondarystat = CriticalChance | Defense | DefensePercentage | Health | HealthPercentage | Offense | OffensePercentage | PotencyPercentage | Protection | ProtectionPercentage | Speed | TenacityPercentage;

type secondarylevel = SecondaryLevel1 | SecondaryLevel2 | SecondaryLevel3 | SecondaryLevel4;

type secondaryvalue = SecondaryFloat (float);

type characterid = CharacterId (string);

type moduid = ModUniqueId (string);

let makeprimaryvalue: (primarystat,primarylevel,pips) => primaryvalue = (primarystat,primarylevel,pips) => 
  switch (primarystat,primarylevel,pips) {
  | (PrimaryAccuracy,PrimaryLevel (x),Pip4)          => PrimaryFloat (0.75 +. 0.65 *. float_of_int (x));  
  | (PrimaryAccuracy,PrimaryLevel (x),Pip5)          => PrimaryFloat (0.75 +. 0.75 *. float_of_int (x));
  | (PrimaryCriticalAvoidance,PrimaryLevel (x),Pip3) => PrimaryFloat (1.0 +. 1.1 *. float_of_int (x));
  | (PrimaryCriticalAvoidance,PrimaryLevel (x),Pip4) => PrimaryFloat (1.5 +. 1.3 *. float_of_int (x));
  | (PrimaryCriticalAvoidance,PrimaryLevel (x),Pip5) => PrimaryFloat (1.5 +. 1.5 *. float_of_int (x));
  | (PrimaryCriticalChance,PrimaryLevel (x),Pip3)    => PrimaryFloat (0.5 +. 0.55 *. float_of_int (x));
  | (PrimaryCriticalChance,PrimaryLevel (x),Pip4)    => PrimaryFloat (0.75 +. 0.65 *. float_of_int (x));
  | (PrimaryCriticalChance,PrimaryLevel (x),Pip5)    => PrimaryFloat (0.75 +. 0.75 *. float_of_int (x));
  | (PrimaryCriticalDamage,PrimaryLevel (x),Pip2)    => PrimaryFloat (1.5 +. 1.5 *. float_of_int (x));
  | (PrimaryCriticalDamage,PrimaryLevel (x),Pip3)    => PrimaryFloat (1.5 +. 1.65 *. float_of_int (x));
  | (PrimaryCriticalDamage,PrimaryLevel (x),Pip4)    => PrimaryFloat (2.25 +. 1.95 *. float_of_int (x));
  | (PrimaryCriticalDamage,PrimaryLevel (x),Pip5)    => PrimaryFloat (2.25 +. 2.25 *. float_of_int (x));
  | (PrimaryDefense,PrimaryLevel (x),Pip3)           => PrimaryFloat (0.25 +. 0.5 *. float_of_int (x));
  | (PrimaryDefense,PrimaryLevel (x),Pip4)           => PrimaryFloat (0.5 +. 0.5 *. float_of_int (x));
  | (PrimaryDefense,PrimaryLevel (x),Pip5)           => PrimaryFloat (0.5 +. 0.75 *. float_of_int (x));
  | (PrimaryHealth,PrimaryLevel (x),Pip3)            => PrimaryFloat (0.13 +. 0.25 *. float_of_int (x));
  | (PrimaryHealth,PrimaryLevel (x),Pip4)            => PrimaryFloat (0.25 +. 0.25 *. float_of_int (x));
  | (PrimaryHealth,PrimaryLevel (x),Pip5)            => PrimaryFloat (0.252667 +. 0.375 *. float_of_int (x));
  | (PrimaryOffense,PrimaryLevel (x),Pip3)           => PrimaryFloat (0.13 +. 0.25 *. float_of_int (x));
  | (PrimaryOffense,PrimaryLevel (x),Pip4)           => PrimaryFloat (0.25 +. 0.25 *. float_of_int (x));
  | (PrimaryOffense,PrimaryLevel (x),Pip5)           => PrimaryFloat (0.5 +. 0.75 *. float_of_int (x));
  | (PrimaryPotency,PrimaryLevel (x),Pip4)           => PrimaryFloat (1.5 +. 1.35 *. float_of_int (x));
  | (PrimaryPotency,PrimaryLevel (x),Pip5)           => PrimaryFloat (1.5 +. 1.5 *. float_of_int (x));
  | (PrimaryProtection,PrimaryLevel (x),Pip3)        => PrimaryFloat (0.5 +. 1.0 *. float_of_int (x));
  | (PrimaryProtection,PrimaryLevel (x),Pip4)        => PrimaryFloat (1.0 +. 1.0 *. float_of_int (x));
  | (PrimaryProtection,PrimaryLevel (x),Pip5)        => PrimaryFloat (1.0 +. 1.5 *. float_of_int (x));
  | (PrimarySpeed,PrimaryLevel (x),Pip4)             => PrimaryFloat (0.00516756 *. float_of_int (x * x) +. (1.55895 *. float_of_int (x) +. 1.44287));
  | (PrimarySpeed,PrimaryLevel (x),Pip5)             => PrimaryFloat ((0.0 -. 0.0028909) *. float_of_int (x * x * x) +. 0.0693816 *. float_of_int (x * x) +. 1.41293 *. float_of_int (x) +. 2.73626);
  | (PrimaryTenacity,PrimaryLevel (x),Pip3)          => PrimaryFloat (1.0 +. 1.1 *. float_of_int (x));
  | (PrimaryTenacity,PrimaryLevel (x),Pip4)          => PrimaryFloat (1.5 +. 1.3 *. float_of_int (x));
  | (PrimaryTenacity,PrimaryLevel (x),Pip5)          => PrimaryFloat (1.5 +. 1.5 *. float_of_int (x));
  | (_,_,_) => PrimaryFloat (0.0)
};

let encodePrimaryStat = pstat =>
  switch (pstat){
  | PrimaryAccuracy          => Aeson.Encode.string ("Accuracy %")
  | PrimaryCriticalAvoidance => Aeson.Encode.string ("Critical Avoidance %")
  | PrimaryCriticalChance    => Aeson.Encode.string ("Critical Chance %")
  | PrimaryCriticalDamage    => Aeson.Encode.string ("Critical Damage %")
  | PrimaryDefense           => Aeson.Encode.string ("Defense %")
  | PrimaryHealth            => Aeson.Encode.string ("Health %")
  | PrimaryOffense           => Aeson.Encode.string ("Offense %")
  | PrimaryPotency           => Aeson.Encode.string ("Potency %")
  | PrimaryProtection        => Aeson.Encode.string ("Protection %")
  | PrimarySpeed             => Aeson.Encode.string ("Speed")
  | PrimaryTenacity          => Aeson.Encode.string ("Tenacity %")
  };

let decodePrimaryStat = json : Belt.Result.t(primarystat,string) => {
  let primstatstr = json |> Aeson.Decode.string;
  switch (primstatstr) {
   | "Accuracy %"           => Belt.Result.Ok(PrimaryAccuracy)
   | "Critical Avoidance %" => Belt.Result.Ok(PrimaryCriticalAvoidance)
   | "Critical Chance %"    => Belt.Result.Ok(PrimaryCriticalChance)
   | "Critical Damage %"    => Belt.Result.Ok(PrimaryCriticalDamage)
   | "Defense %"            => Belt.Result.Ok(PrimaryDefense)
   | "Health %"             => Belt.Result.Ok(PrimaryHealth)
   | "Offense %"            => Belt.Result.Ok(PrimaryOffense)
   | "Potency %"            => Belt.Result.Ok(PrimaryPotency)
   | "Protection %"         => Belt.Result.Ok(PrimaryProtection)
   | "Speed"                => Belt.Result.Ok(PrimarySpeed)
   | "Tenacity %"           => Belt.Result.Ok(PrimaryTenacity)
   | str => Belt.Result.Error("Instead of a PrimaryStat, received: " ++ str)
   | exception (Aeson.Decode.DecodeError(msg)) => Belt.Result.Error(msg)
  };
};

let primaryStatToString : primarystat => string = pstat =>
   switch (pstat) {
   | PrimaryAccuracy          => " Primary Stat: Accuracy %"
   | PrimaryCriticalAvoidance => " Primary Stat: Critical Avoidance %"
   | PrimaryCriticalChance    => " Primary Stat: Critical Chance %"
   | PrimaryCriticalDamage    => " Primary Stat: Critical Damage %"
   | PrimaryDefense           => " Primary Stat: Defense %"
   | PrimaryHealth            => " Primary Stat: Health %"  
   | PrimaryOffense           => " Primary Stat: Offense %"
   | PrimaryPotency           => " Primary Stat: Potency %"
   | PrimaryProtection        => " Primary Stat: Protection %"
   | PrimarySpeed             => " Primary Stat: Speed"
   | PrimaryTenacity          => " Primary Stat: Tenacity %"
   };

/* ----------------------------------------------------------------------- */
let truncatedFloatTo2DigitsString : float => string = floatval =>
  Js.Float.toFixedWithPrecision (floatval, ~digits=2);

let truncatedFloatToIntString : float => string = floatval =>
  Js.Float.toFixedWithPrecision (floatval, ~digits=0);

let stringifyValue : (primarystat,primaryvalue) => string = (pstat,pval) =>
  switch (pstat,pval) {
  | (PrimarySpeed,PrimaryFloat (fval)) => truncatedFloatToIntString (fval)
  | (_,PrimaryFloat (fval)) => truncatedFloatTo2DigitsString (fval)
  };

let encodePrimaryValue = (pstat : primarystat, pval : primaryvalue) : Js.Json.t => {
  let stringvalue = stringifyValue (pstat,pval);
  Aeson.Encode.string("+" ++ stringvalue)
  };

let decodePrimaryValue = json : Belt.Result.t(primaryvalue,string) => {
  let primaryValueString = json |> Aeson.Decode.string
  switch (primaryValueString) {
    | ""  => Belt.Result.Error("Expected a PrimaryValue, but received and empty value")
    | str => Belt.Result.Ok(PrimaryFloat (Js.Float.fromString (str)))
  };
};

let stringifyPrimaryValue : primaryvalue => string = (PrimaryFloat (pv)) => " PrimaryValue: " ++ Js.Float.toString (pv);
/* ----------------------------------------------------------------------- */
let encodePips = (p : pips) : Js.Json.t => {
  switch (p) {
  | Pip1 => Aeson.Encode.int (1)
  | Pip2 => Aeson.Encode.int (2)
  | Pip3 => Aeson.Encode.int (3)
  | Pip4 => Aeson.Encode.int (4)
  | Pip5 => Aeson.Encode.int (5)
  };
};

let decodePips = json : Belt.Result.t(pips,string) => {
  let pipint = json |> Aeson.Decode.int
  switch (pipint) {
  | 1 => Belt.Result.Ok(Pip1)
  | 2 => Belt.Result.Ok(Pip2)
  | 3 => Belt.Result.Ok(Pip3)
  | 4 => Belt.Result.Ok(Pip4)
  | 5 => Belt.Result.Ok(Pip5)
  | x => Belt.Result.Error("Expected a Pip number 1-5, but instead received: " ++ Js.Int.toString (x))
  };
};

let stringifyPips : pips => string = p =>
  switch (p) {
  | Pip1 => " pips: Pip1"
  | Pip2 => " pips: Pip2"
  | Pip3 => " pips: Pip3"
  | Pip4 => " pips: Pip4"
  | Pip5 => " pips: Pip5"
};
/* ----------------------------------------------------------------------- */
let encodeShape = (shp : shape) : Js.Json.t =>
  switch (shp) {
  | Square   => Aeson.Encode.string ("square")
  | Arrow    => Aeson.Encode.string ("arrow")
  | Diamond  => Aeson.Encode.string ("diamond")
  | Triangle => Aeson.Encode.string ("triangle")
  | Circle   => Aeson.Encode.string ("circle")
  | Cross    => Aeson.Encode.string ("cross")
  };

let decodeShape = json : Belt.Result.t(shape,string) => {
  let shp = json |> Aeson.Decode.string
  switch (shp) {
  | "square"   => Belt.Result.Ok(Square)
  | "arrow"    => Belt.Result.Ok(Arrow)
  | "diamond"  => Belt.Result.Ok(Diamond)
  | "triangle" => Belt.Result.Ok(Triangle)
  | "circle"   => Belt.Result.Ok(Circle)
  | "cross"    => Belt.Result.Ok(Cross)
  | str => Belt.Result.Error("Expected a shape, but instead received: " ++ str)
  };
};

let stringifyShape : shape => string = shp =>
  switch (shp) {
  | Square   => " Shape: Square"
  | Arrow    => " Shape: Arrow"
  | Diamond  => " Shape: Diamond"
  | Triangle => " Shape: Triangle"
  | Circle   => " Shape: Circle"
  | Cross    => " Shape: Cross"
  };
/* ----------------------------------------------------------------------- */
let encodeStatSet = (sset : statset) : Js.Json.t =>
  switch (sset) {
  | CriticalChanceSet => Aeson.Encode.string ("critchance")
  | CriticalDamageSet => Aeson.Encode.string ("critdamage")
  | DefenseSet        => Aeson.Encode.string ("defense")
  | HealthSet         => Aeson.Encode.string ("health")
  | OffenseSet        => Aeson.Encode.string ("offense")
  | PotencySet        => Aeson.Encode.string ("potency")
  | SpeedSet          => Aeson.Encode.string ("speed")
  | TenacitySet       => Aeson.Encode.string ("tenacity")
  };

let decodeStatSet = json : Belt.Result.t(statset,string) => {
  let sset = json |> Aeson.Decode.string
  switch (sset) {
  | "critchance" => Belt.Result.Ok(CriticalChanceSet)
  | "critdamage" => Belt.Result.Ok(CriticalDamageSet)
  | "defense"    => Belt.Result.Ok(DefenseSet)
  | "health"     => Belt.Result.Ok(HealthSet)
  | "offense"    => Belt.Result.Ok(OffenseSet)
  | "potency"    => Belt.Result.Ok(PotencySet)
  | "speed"      => Belt.Result.Ok(SpeedSet)
  | "tenacity"   => Belt.Result.Ok(TenacitySet)
  | str => Belt.Result.Error("Expected a statset, but instead received: " ++ str)
  };
};

let stringifyStatSet : statset => string = sset =>
  switch (sset) {
  | CriticalChanceSet => " Set: Crit Chance"
  | CriticalDamageSet => " Set: Crit Damage"
  | DefenseSet        => " Set: Defense"
  | HealthSet         => " Set: Health"
  | OffenseSet        => " Set: Offense"
  | PotencySet        => " Set: Potency"
  | SpeedSet          => " Set: Speed"
  | TenacitySet       => " Set: Tenacity"
  };
/* ----------------------------------------------------------------------- */
let encodePrimaryLevel = ((PrimaryLevel (plvl)) : primarylevel) : Js.Json.t =>
  Aeson.Encode.int (plvl);

let decodePrimaryLevel = json : Belt.Result.t(primarylevel,string) => {
  let lvl = json |> Aeson.Decode.int
  switch (lvl) {
  | ilvl when (ilvl < 1)  => Belt.Result.Error("Unexpected a primarylevel outside range 1-15: " ++ Js.Int.toString (ilvl))
  | ilvl when (ilvl > 15) => Belt.Result.Error("Unexpected a primarylevel outside range 1-15: " ++ Js.Int.toString (ilvl))
  | ilvl => Belt.Result.Ok(PrimaryLevel (ilvl))
  };
};

let stringifyPrimaryLevel : primarylevel => string = (PrimaryLevel (plvl)) => "Level: " ++ Js.Int.toString (plvl); 
/* ----------------------------------------------------------------------- */
let encodeTier = (color : tier) : Js.Json.t =>
  switch (color) {
  | Grey   => Aeson.Encode.int (1)
  | Green  => Aeson.Encode.int (2)
  | Blue   => Aeson.Encode.int (3)
  | Purple => Aeson.Encode.int (4)
  | Gold   => Aeson.Encode.int (5)
  };

let decodeTier = json : Belt.Result.t(tier,string) => {
  let ntier = json |> Aeson.Decode.int
  switch (ntier) {
  | 1 => Belt.Result.Ok(Grey)
  | 2 => Belt.Result.Ok(Green)
  | 3 => Belt.Result.Ok(Blue)
  | 4 => Belt.Result.Ok(Purple)
  | 5 => Belt.Result.Ok(Gold)
  | x => Belt.Result.Error("Unexpected tier outside range 1-5: " ++ Js.Int.toString (x))
  };
};

let stringifyTier : tier => string = color =>
  switch (color) {
  | Grey   => " Tier: Grey"
  | Green  => " Tier: Green"
  | Blue   => " Tier: Blue"
  | Purple => " Tier: Purple"
  | Gold   => " Tier: Gold"
  };
/* ----------------------------------------------------------------------- */
let encodeCharacterId = ((CharacterId (charid)) : characterid) : Js.Json.t => Aeson.Encode.string (charid);

let decodeCharacterId = json : Belt.Result.t(characterid,string) => {
  let charid = json |> Aeson.Decode.string
  Belt.Result.Ok(CharacterId (charid))
};

let stringifyCharacterId : characterid => string = (CharacterId (charid)) => " CharacterId: " ++ charid;
/* ----------------------------------------------------------------------- */
let makeModUId : string = {
  let randomuid36 = Uuid.V4.uuidv4();
  let uid18 = String.sub(randomuid36,0,18);
  uid18 ++ "5168";
  };

let encodeModUId = (maybemoduid : option(moduid)) : Js.Json.t =>
  switch (maybemoduid) {
  | Some (ModUniqueId (modId)) => Aeson.Encode.string (modId);
  | None => Aeson.Encode.string (makeModUId);
  };        

let decodeModUId = json : Belt.Result.t(moduid,string) => {
  let modId = json |> Aeson.Decode.string
  Belt.Result.Ok(ModUniqueId (modId))
};

let stringifyModUId : moduid => string = (ModUniqueId (modId)) => " ModUId: " ++ modId;

/* ----------------------------------------------------------------------- */
let encodeSecondaryStat = (maybesstat : option(secondarystat)) : Js.Json.t =>
  switch (maybesstat) {
  | None => Aeson.Encode.string ("")
  | Some (CriticalChance)       => Aeson.Encode.string ("Critical Chance %")
  | Some (Defense)              => Aeson.Encode.string ("Defense")
  | Some (DefensePercentage)    => Aeson.Encode.string ("Defense %")
  | Some (Health)               => Aeson.Encode.string ("Health")
  | Some (HealthPercentage)     => Aeson.Encode.string ("Health %")
  | Some (Offense)              => Aeson.Encode.string ("Offense")
  | Some (OffensePercentage)    => Aeson.Encode.string ("Offense %")
  | Some (PotencyPercentage)    => Aeson.Encode.string ("Potency %")
  | Some (Protection)           => Aeson.Encode.string ("Protection")
  | Some (ProtectionPercentage) => Aeson.Encode.string ("Protection %")
  | Some (Speed)                => Aeson.Encode.string ("Speed")
  | Some (TenacityPercentage)   => Aeson.Encode.string ("Tenacity %")
  };

let decodeSecondaryStat = json : Belt.Result.t(option(secondarystat),string) => {
  let sstat = json |> Aeson.Decode.string
  switch (sstat) {
  | "Critical Chance %" => Belt.Result.Ok(Some(CriticalChance))
  | "Defense"           => Belt.Result.Ok(Some(Defense))               
  | "Defense %"         => Belt.Result.Ok(Some(DefensePercentage))     
  | "Health"            => Belt.Result.Ok(Some(Health))                
  | "Health %"          => Belt.Result.Ok(Some(HealthPercentage))      
  | "Offense"           => Belt.Result.Ok(Some(Offense))               
  | "Offense %"         => Belt.Result.Ok(Some(OffensePercentage))     
  | "Potency %"         => Belt.Result.Ok(Some(PotencyPercentage))     
  | "Protection"        => Belt.Result.Ok(Some(Protection))            
  | "Protection %"      => Belt.Result.Ok(Some(ProtectionPercentage))  
  | "Speed"             => Belt.Result.Ok(Some(Speed))                 
  | "Tenacity %"        => Belt.Result.Ok(Some(TenacityPercentage))
  | ""                  => Belt.Result.Ok(None)
  | str => Belt.Result.Error("Expected a secondarystat, but instead received: " ++ str)
  };
};

let stringifySecondaryStat : option(secondarystat) => string = ssecstat =>
  switch (ssecstat) {
  | Some (CriticalChance)       => " Secondary Stat: Critical Chance %"
  | Some (Defense)              => " Secondary Stat: Defense"
  | Some (DefensePercentage)    => " Secondary Stat: Defense %"
  | Some (Health)               => " Secondary Stat: Health"
  | Some (HealthPercentage)     => " Secondary Stat: Health %"
  | Some (Offense)              => " Secondary Stat: Offense"
  | Some (OffensePercentage)    => " Secondary Stat: Offense %"
  | Some (PotencyPercentage)    => " Secondary Stat: Potency %"
  | Some (Protection)           => " Secondary Stat: Protection"
  | Some (ProtectionPercentage) => " Secondary Stat: Protection %"
  | Some (Speed)                => " Secondary Stat: Speed"
  | Some (TenacityPercentage)   => " Secondary Stat: Tenacity %"
  | None                        => " Secondary Stat: None"
  };
/* ----------------------------------------------------------------------- */
let encodeSecondaryValue = (maybeval : option(secondaryvalue)) : Js.Json.t =>
  switch (maybeval) {
  | None => Aeson.Encode.string ("")
  | Some (SecondaryFloat (x)) => Aeson.Encode.string ("+" ++ Js.Float.toString(x))
  };

let decodeSecondaryValue = json : Belt.Result.t(option(secondaryvalue),string) => {
  let svalue = json |> Aeson.Decode.string
  switch (svalue) {
  | ""  => Belt.Result.Ok(None)
  | str => Belt.Result.Ok (Some (SecondaryFloat (Js.Float.fromString (str))))
  }
};
      
let stringifySecondaryValue : option(secondaryvalue) => string = maybevalue =>
  switch (maybevalue) {
  | None => " Secondary Value: None"
  | Some (SecondaryFloat (flt)) => " Secondary Value: " ++ Js.Float.toString (flt)
  };
/* ----------------------------------------------------------------------- */
let encodeSecondaryLevel = (maybelvl : option(secondarylevel)) : Js.Json.t =>
  switch (maybelvl) {
  | None => Aeson.Encode.string ("")
  | Some (SecondaryLevel1) => Aeson.Encode.int (1)
  | Some (SecondaryLevel2) => Aeson.Encode.int (2)
  | Some (SecondaryLevel3) => Aeson.Encode.int (3)
  | Some (SecondaryLevel4) => Aeson.Encode.int (4)
  };

let decodeSecondaryLevel = json : Belt.Result.t(option(secondarylevel),string) => {
  let decodeStr = js => Aeson.Decode.string (js) |> ( _ => None);
  let decodeInt = js => Aeson.Decode.int (js) |> (lvl => Some (lvl));
  let maybelvl = Aeson.Decode.oneOf([decodeStr,decodeInt],json);
  switch (maybelvl) {
  | None => Belt.Result.Ok(None)
  | Some (1) => Belt.Result.Ok(Some (SecondaryLevel1))
  | Some (2) => Belt.Result.Ok(Some (SecondaryLevel2))
  | Some (3) => Belt.Result.Ok(Some (SecondaryLevel3))
  | Some (4) => Belt.Result.Ok(Some (SecondaryLevel4))
  | Some (x) => Belt.Result.Error("Unexpected a secondarylevel outside range 1-4: " ++ Js.Int.toString (x))
  }
};

let stringifySecondaryLevel : option(secondarylevel) => string = maybelvl =>
  switch (maybelvl) {
  | None => " Secondary Level: None"
  | Some (SecondaryLevel1) => " Secondary Level: 1"
  | Some (SecondaryLevel2) => " Secondary Level: 2"
  | Some (SecondaryLevel3) => " Secondary Level: 3"
  | Some (SecondaryLevel4) => " Secondary Level: 4"
  };

/* ----------------------------------------------------------------------- */

type cmod = {
  primstat: primarystat,
  primvalue: primaryvalue,
  secondarystat1: option(secondarystat),
  secondaryvalue1: option(secondaryvalue),
  secondarylevel1: option(secondarylevel),
  secondarystat2: option(secondarystat),
  secondaryvalue2: option(secondaryvalue),
  secondarylevel2: option(secondarylevel),
  secondarystat3: option(secondarystat),
  secondaryvalue3: option(secondaryvalue),
  secondarylevel3: option(secondarylevel),
  secondarystat4: option(secondarystat),
  secondaryvalue4: option(secondaryvalue),
  secondarylevel4: option(secondarylevel),
  primpips: pips,
  primshape: shape,
  primset: statset,
  primlevel: primarylevel,
  primtier: tier,
  primchar: option(characterid),
  primmodid: option(moduid),
};    

let encodeCharMod = (cm: cmod) : Js.Json.t => {
  Aeson.Encode.object_ ([
  ("primaryBonusType", encodePrimaryStat (cm.primstat)),
  ("primaryBonusValue", encodePrimaryValue (cm.primstat,cm.primvalue)),
  ("secondaryType_1", encodeSecondaryStat (cm.secondarystat1)),
  ("secondaryValue_1", encodeSecondaryValue (cm.secondaryvalue1)),
  ("secondaryRoll_1", encodeSecondaryLevel (cm.secondarylevel1)),
  ("secondaryType_2", encodeSecondaryStat (cm.secondarystat2)),
  ("secondaryValue_2", encodeSecondaryValue (cm.secondaryvalue2)),
  ("secondaryRoll_2", encodeSecondaryLevel (cm.secondarylevel2)),
  ("secondaryType_3", encodeSecondaryStat (cm.secondarystat3)),
  ("secondaryValue_3", encodeSecondaryValue (cm.secondaryvalue3)),
  ("secondaryRoll_3", encodeSecondaryLevel (cm.secondarylevel3)),
  ("secondaryType_4", encodeSecondaryStat (cm.secondarystat4)),
  ("secondaryValue_4", encodeSecondaryValue (cm.secondaryvalue4)),
  ("secondaryRoll_4", encodeSecondaryLevel (cm.secondarylevel4)),
  ("pips", encodePips (cm.primpips)),
  ("slot", encodeShape (cm.primshape)),
  ("set", encodeStatSet (cm.primset)),
  ("level", encodePrimaryLevel (cm.primlevel)),
  ("tier", encodeTier (cm.primtier)),
  ("characterID", Aeson.Encode.optional(encodeCharacterId,cm.primchar)),
  ("mod_uid", encodeModUId (cm.primmodid)),
  ]);
};

let decodeCharMod = json : cmod =>
  Aeson.Decode.{
  primstat: json |> field("primaryBonusType", decodePrimaryStat) |> Aeson.Decode.unwrapResult,
  primvalue: json |> field("primaryBonusValue", decodePrimaryValue) |> Aeson.Decode.unwrapResult,
  secondarystat1: json |> field ("secondaryType_1", decodeSecondaryStat)|> Aeson.Decode.unwrapResult,
  secondaryvalue1: json |> field ("secondaryValue_1", decodeSecondaryValue) |> Aeson.Decode.unwrapResult,
  secondarylevel1: json |> field ("secondaryRoll_1", decodeSecondaryLevel) |> Aeson.Decode.unwrapResult,
  secondarystat2: json |> field ("secondaryType_2", decodeSecondaryStat)|> Aeson.Decode.unwrapResult,
  secondaryvalue2: json |> field ("secondaryValue_2", decodeSecondaryValue) |> Aeson.Decode.unwrapResult,
  secondarylevel2: json |> field ("secondaryRoll_2", decodeSecondaryLevel) |> Aeson.Decode.unwrapResult,
  secondarystat3: json |> field ("secondaryType_3", decodeSecondaryStat)|> Aeson.Decode.unwrapResult,
  secondaryvalue3: json |> field ("secondaryValue_3", decodeSecondaryValue) |> Aeson.Decode.unwrapResult,
  secondarylevel3: json |> field ("secondaryRoll_3", decodeSecondaryLevel) |> Aeson.Decode.unwrapResult,
  secondarystat4: json |> field ("secondaryType_4", decodeSecondaryStat)|> Aeson.Decode.unwrapResult,
  secondaryvalue4: json |> field ("secondaryValue_4", decodeSecondaryValue) |> Aeson.Decode.unwrapResult,
  secondarylevel4: json |> field ("secondaryRoll_4", decodeSecondaryLevel) |> Aeson.Decode.unwrapResult,
  primmodid: json |> field("mod_uid", Aeson.Decode.optional(x => decodeModUId (x)|> Aeson.Decode.unwrapResult)),
  primshape: json |> field("slot", decodeShape) |> Aeson.Decode.unwrapResult,
  primset: json |> field("set",decodeStatSet) |> Aeson.Decode.unwrapResult,
  primlevel: json |> field("level", decodePrimaryLevel) |> Aeson.Decode.unwrapResult,
  primpips: json |> field("pips", decodePips) |> Aeson.Decode.unwrapResult,
  primchar: json |> field("characterID", Aeson.Decode.optional(x => decodeCharacterId (x) |> Aeson.Decode.unwrapResult)),
  primtier: json |> field("tier", decodeTier) |> Aeson.Decode.unwrapResult,
  };

let stringifyCharMod : cmod => string = cm => {
  let strstat = primaryStatToString (cm.primstat);
  let strval = stringifyPrimaryValue (cm.primvalue);
  let strpips = stringifyPips (cm.primpips);
  let strshape = stringifyShape (cm.primshape);
  let strset = stringifyStatSet (cm.primset);
  let strlvl = stringifyPrimaryLevel (cm.primlevel);
  let strtier = stringifyTier (cm.primtier);
  let strsstat1 = stringifySecondaryStat (cm.secondarystat1);
  let strsvalue1 = stringifySecondaryValue (cm.secondaryvalue1);
  let strslevel1 = stringifySecondaryLevel (cm.secondarylevel1);
  let strsstat2 = stringifySecondaryStat (cm.secondarystat2);
  let strsvalue2 = stringifySecondaryValue (cm.secondaryvalue2);
  let strslevel2 = stringifySecondaryLevel (cm.secondarylevel2);
  let strsstat3 = stringifySecondaryStat (cm.secondarystat3);
  let strsvalue3 = stringifySecondaryValue (cm.secondaryvalue3);
  let strslevel3 = stringifySecondaryLevel (cm.secondarylevel3);
  let strsstat4 = stringifySecondaryStat (cm.secondarystat4);
  let strsvalue4 = stringifySecondaryValue (cm.secondaryvalue4);
  let strslevel4 = stringifySecondaryLevel (cm.secondarylevel4);
  let strchar = switch (cm.primchar) {
    | None => " CharacterId: None"
    | Some (charid) => stringifyCharacterId (charid)};
  let strmoduid = switch (cm.primmodid) {
    | None => " ModUniqueId: None"
    | Some (modid) => stringifyModUId (modid)};
  strstat ++ strval ++ strpips ++ strshape ++ strset ++ strlvl ++ strtier ++ strchar ++ strmoduid ++ strsstat1 ++ strsvalue1 ++ strslevel1 ++ strsstat2 ++ strsvalue2 ++ strslevel2 ++ strsstat3 ++ strsvalue3 ++ strslevel3 ++ strsstat4 ++ strsvalue4 ++ strslevel4
};    
/* ----------------------------------------------------------------------- */
let encodeMods = (modlist : list(cmod)) : Js.Json.t =>
  Aeson.Encode.object_ ([
  ("mods", modlist |> Aeson.Encode.list(encodeCharMod))
  ]);

let decodeMods = json : list(cmod) => json |> Aeson.Decode.field ("mods", Aeson.Decode.list(decodeCharMod));

let testJson = {|{"mods":[{"primaryBonusType":"Offense %","primaryBonusValue":"+3.63","secondaryType_1":"Health","secondaryValue_1":"+296","secondaryRoll_1":1,"secondaryType_2":"Defense","secondaryValue_2":"+8","secondaryRoll_2":1,"secondaryType_3":"Health %","secondaryValue_3":"+0.83","secondaryRoll_3":1,"secondaryType_4":"Protection %","secondaryValue_4":"+1.16","secondaryRoll_4":1,"mod_uid":"Bisg176CqrTNGQ3I845168","slot":"square","set":"critdamage","level":9,"pips":5,"characterID":null,"tier":2},{"primaryBonusType":"Offense %","primaryBonusValue":"+2.5","secondaryType_1":"Defense","secondaryValue_1":"+5","secondaryRoll_1":1,"secondaryType_2":"Offense","secondaryValue_2":"+33","secondaryRoll_2":1,"secondaryType_3":"","secondaryValue_3":"","secondaryRoll_3":"","secondaryType_4":"","secondaryValue_4":"","secondaryRoll_4":"","mod_uid":"Vh4aid35x7kCIo1m5Y5168","slot":"square","set":"critdamage","level":6,"pips":5,"characterID":null,"tier":1},{"primaryBonusType":"Offense %","primaryBonusValue":"+2.5","secondaryType_1":"Potency %","secondaryValue_1":"+1.77","secondaryRoll_1":1,"secondaryType_2":"Tenacity %","secondaryValue_2":"+1.64","secondaryRoll_2":1,"secondaryType_3":"","secondaryValue_3":"","secondaryRoll_3":"","secondaryType_4":"","secondaryValue_4":"","secondaryRoll_4":"","mod_uid":"eDo852wRUhufzaz36H5168","slot":"square","set":"critdamage","level":6,"pips":5,"characterID":null,"tier":1}]}|}
let readjson = Js.Json.parseExn(Node.Fs.readFileAsUtf8Sync ("test.json"));
let testPrim : cmod = { primstat: PrimaryDefense, primvalue: PrimaryFloat (2.5389), primpips: Pip2, primshape: Square, primset: CriticalChanceSet, primlevel: PrimaryLevel (12), primtier: Blue, primchar: None, primmodid: None, secondarystat1: Some(Defense), secondaryvalue1: Some(SecondaryFloat (1.2)), secondarylevel1: (Some (SecondaryLevel2)), secondarystat2: None, secondaryvalue2: None, secondarylevel2: None, secondarystat3: None, secondaryvalue3: None, secondarylevel3: None, secondarystat4: None, secondaryvalue4: None, secondarylevel4: None };
let testValue = encodePrimaryValue (PrimarySpeed,PrimaryFloat (2.45));
let jsonResultToString : Belt.Result.t(primarystat,string) => string = t =>
  switch (t) {
    | Ok(pstat) => primaryStatToString (pstat)
    | Error(msg) => msg
  };

let encPrim = encodeCharMod (testPrim);
let decPrim = decodeCharMod (encPrim);
let strPrim = stringifyCharMod (decPrim);

let decodedmods = decodeMods (Js.Json.parseExn(testJson));
let encodedmods = Js.Json.stringify (encodeMods (decodedmods));


let emptyArray = [||];

let arrayOfOptionsToOptionArray : array(option ('a)) => option(array('a)) = arrayOfOptions => {
  let fcn = (maybeArray : option(array('a)), maybeElem : option('a)) =>
    switch (maybeArray,maybeElem) {
    | (Some (x),Some(y)) => Some (Array.append(x, Array.make (1,y)))
    | (_,_) => None
    };
  Array.fold_left(fcn,Some(emptyArray),arrayOfOptions);
};
  
let updateObj : (Js.Dict.t(Js.Json.t)) => (Js.Json.t) = profileObjectDict => {
  Js.Dict.set(profileObjectDict, "mods", Aeson.Encode.array (emptyArray));
  Js.Json.object_ (profileObjectDict);
};

let getMods : Js.Json.t => option(Js.Json.t) = mainjson => {
  let obj = {
    switch (Js.Json.decodeObject(mainjson)) {
    | None => None
    | Some (mainObjectDict : Js.Dict.t(Js.Json.t)) =>
      switch (Js.Dict.get(mainObjectDict,"profiles")) {
      | None => None
      | Some (profilesValueAsJsont : Js.Json.t) =>
        switch (Js.Json.decodeArray (profilesValueAsJsont)) {
        | None => None
        | Some (profileValueAsArrayJsont : array(Js.Json.t)) =>
          let arrayOfMaybeDict : array(option(Js.Dict.t(Js.Json.t))) = Array.map(Js.Json.decodeObject,profileValueAsArrayJsont)
          let maybeArrayOfDict : option(array(Js.Dict.t(Js.Json.t))) = arrayOfOptionsToOptionArray(arrayOfMaybeDict)
          switch (maybeArrayOfDict) {
          | None => None
          | Some (arrayOfDictOfObjects : array(Js.Dict.t(Js.Json.t))) => {
              let updatedProfileObjectArray = Array.map(updateObj,arrayOfDictOfObjects)
              let updatedProfileValue = Aeson.Encode.array(updatedProfileObjectArray)
              Js.Dict.set(mainObjectDict,"profiles",updatedProfileValue);  
              Some (mainObjectDict);
              };
          };
        };
      };
    };
  };
  Belt.Option.map(obj, Js.Json.object_);
};

let stringifyOptionJson : option(Js.Json.t) => string = maybejson =>
  switch (maybejson) {
  | None => "None"
  | Some (jsonobj) => Js.Json.stringify (jsonobj)
  };

/* let testJson = List.map (stringifyCharMod,decodedmods); */
/* Js.log(testJson); */
/* let testDecodeJson = decodePrimary(readjson); */
Js.log (stringifyOptionJson (getMods(readjson)));
/* Js.log (readjson); */
/* Js.log (stringifyOptionJson (updateObj)); */
/* Js.log (strPrim); */
/* Js.log (encodedmods); */
/* Js.log (stringifyTestPrimary(testDecodeJson)); */
let test = makeprimaryvalue (PrimarySpeed,PrimaryLevel (15),Pip5);

/*
Node.Fs.readFileAsUtf8Sync "package.json"

val readFileAsUtf8Sync : string -> string
val writeFileAsUtf8Sync : string -> string -> unit

val readFileSync : string ->
       [ `ascii | `base64 | `binary | `hex | `latin1 | `ucs2 | `utf16le | `utf8 ] ->
       string
       
val writeFileSync : string ->
       string ->
       [ `ascii | `base64 | `binary | `hex | `latin1 | `ucs2 | `utf16le | `utf8 ] ->
       unit

let testStat = encodePrimaryStat (PrimaryAccuracy) |> Js.Json.stringify;
Js.log (jsonResultToString (decodePrimaryStat (testStat)));
Node.Fs.writeFileAsUtf8Sync ("test.json",testStat);
Js.log (decodePrimaryStat (testStat));
 */
