
# 2 "BoolCalc.yrd.fs"
module BoolCalc.Parser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode

# 1 "BoolCalc.yrd"

open BoolCalc.AST

# 14 "BoolCalc.yrd.fs"
type Token =
    | AND of (string)
    | DIV of (string)
    | ELSE of (string)
    | EQ of (string)
    | FALSE of (string)
    | GE of (string)
    | GR of (string)
    | IF of (string)
    | LBRACE of (string)
    | LE of (string)
    | LS of (string)
    | MINUS of (string)
    | MULT of (string)
    | NOT of (string)
    | NOTEQ of (string)
    | NUMBER of (string)
    | OR of (string)
    | PLUS of (string)
    | POW of (string)
    | RBRACE of (string)
    | RNGLR_EOF of (string)
    | SEMI of (string)
    | THEN of (string)
    | TRUE of (string)
    | VAR of (string)

let genLiteral (str : string) (data : string) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | AND x -> box x
    | DIV x -> box x
    | ELSE x -> box x
    | EQ x -> box x
    | FALSE x -> box x
    | GE x -> box x
    | GR x -> box x
    | IF x -> box x
    | LBRACE x -> box x
    | LE x -> box x
    | LS x -> box x
    | MINUS x -> box x
    | MULT x -> box x
    | NOT x -> box x
    | NOTEQ x -> box x
    | NUMBER x -> box x
    | OR x -> box x
    | PLUS x -> box x
    | POW x -> box x
    | RBRACE x -> box x
    | RNGLR_EOF x -> box x
    | SEMI x -> box x
    | THEN x -> box x
    | TRUE x -> box x
    | VAR x -> box x

let numToString = function
    | 0 -> "and"
    | 1 -> "andExpr"
    | 2 -> "boolCond"
    | 3 -> "boolExpr"
    | 4 -> "compOp"
    | 5 -> "error"
    | 6 -> "expr"
    | 7 -> "factor"
    | 8 -> "factorOp"
    | 9 -> "or"
    | 10 -> "orExpr"
    | 11 -> "powExpr"
    | 12 -> "powOp"
    | 13 -> "program"
    | 14 -> "stmt"
    | 15 -> "term"
    | 16 -> "termOp"
    | 17 -> "yard_rule_binExpr_1"
    | 18 -> "yard_rule_binExpr_3"
    | 19 -> "yard_rule_binExpr_5"
    | 20 -> "yard_rule_boolCondMeta_7"
    | 21 -> "yard_rule_boolCondMeta_9"
    | 22 -> "yard_rule_yard_many_1_2"
    | 23 -> "yard_rule_yard_many_1_4"
    | 24 -> "yard_rule_yard_many_1_6"
    | 25 -> "yard_rule_yard_many_2_10"
    | 26 -> "yard_rule_yard_many_2_8"
    | 27 -> "yard_some_1"
    | 28 -> "yard_start_rule"
    | 29 -> "AND"
    | 30 -> "DIV"
    | 31 -> "ELSE"
    | 32 -> "EQ"
    | 33 -> "FALSE"
    | 34 -> "GE"
    | 35 -> "GR"
    | 36 -> "IF"
    | 37 -> "LBRACE"
    | 38 -> "LE"
    | 39 -> "LS"
    | 40 -> "MINUS"
    | 41 -> "MULT"
    | 42 -> "NOT"
    | 43 -> "NOTEQ"
    | 44 -> "NUMBER"
    | 45 -> "OR"
    | 46 -> "PLUS"
    | 47 -> "POW"
    | 48 -> "RBRACE"
    | 49 -> "RNGLR_EOF"
    | 50 -> "SEMI"
    | 51 -> "THEN"
    | 52 -> "TRUE"
    | 53 -> "VAR"
    | _ -> ""

let tokenToNumber = function
    | AND _ -> 29
    | DIV _ -> 30
    | ELSE _ -> 31
    | EQ _ -> 32
    | FALSE _ -> 33
    | GE _ -> 34
    | GR _ -> 35
    | IF _ -> 36
    | LBRACE _ -> 37
    | LE _ -> 38
    | LS _ -> 39
    | MINUS _ -> 40
    | MULT _ -> 41
    | NOT _ -> 42
    | NOTEQ _ -> 43
    | NUMBER _ -> 44
    | OR _ -> 45
    | PLUS _ -> 46
    | POW _ -> 47
    | RBRACE _ -> 48
    | RNGLR_EOF _ -> 49
    | SEMI _ -> 50
    | THEN _ -> 51
    | TRUE _ -> 52
    | VAR _ -> 53

let isLiteral = function
    | AND _ -> false
    | DIV _ -> false
    | ELSE _ -> false
    | EQ _ -> false
    | FALSE _ -> false
    | GE _ -> false
    | GR _ -> false
    | IF _ -> false
    | LBRACE _ -> false
    | LE _ -> false
    | LS _ -> false
    | MINUS _ -> false
    | MULT _ -> false
    | NOT _ -> false
    | NOTEQ _ -> false
    | NUMBER _ -> false
    | OR _ -> false
    | PLUS _ -> false
    | POW _ -> false
    | RBRACE _ -> false
    | RNGLR_EOF _ -> false
    | SEMI _ -> false
    | THEN _ -> false
    | TRUE _ -> false
    | VAR _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|13; 28; 27; 27; 14; 14; 6; 17; 22; 22; 16; 16; 15; 18; 23; 23; 8; 8; 7; 19; 24; 24; 12; 11; 11; 11; 11; 3; 2; 20; 26; 26; 9; 10; 21; 25; 25; 0; 1; 1; 1; 1; 1; 4; 4; 4; 4; 4; 4|]
let private rules = [|27; 13; 14; 50; 14; 50; 27; 53; 32; 6; 6; 17; 15; 22; 16; 15; 22; 46; 40; 18; 7; 23; 8; 7; 23; 41; 30; 19; 11; 24; 12; 11; 24; 47; 44; 53; 37; 6; 48; 3; 36; 2; 51; 6; 31; 6; 20; 10; 26; 9; 10; 26; 45; 21; 1; 25; 0; 1; 25; 29; 6; 4; 6; 37; 2; 48; 52; 33; 42; 2; 32; 43; 35; 34; 39; 38|]
let private rulesStart = [|0; 1; 2; 4; 7; 10; 11; 12; 14; 14; 17; 18; 19; 20; 22; 22; 25; 26; 27; 28; 30; 30; 33; 34; 35; 36; 39; 40; 46; 47; 49; 49; 52; 53; 54; 56; 56; 59; 60; 63; 66; 67; 68; 70; 71; 72; 73; 74; 75; 76|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber None leftSide)

let private lists_gotos = [|1; 2; 3; 9; 68; 69; 28; 42; 34; 35; 75; 14; 36; 39; 72; 4; 44; 7; 8; 5; 40; 6; 10; 43; 13; 11; 12; 15; 23; 20; 57; 65; 62; 51; 52; 55; 66; 16; 63; 19; 17; 18; 21; 45; 46; 47; 48; 49; 50; 22; 24; 25; 26; 27; 29; 41; 32; 33; 30; 31; 37; 38; 53; 67; 54; 56; 58; 64; 61; 59; 60; 70; 71; 73; 74|]
let private small_gotos =
        [|15; 196608; 393217; 458754; 720899; 851972; 917509; 983046; 1114119; 1179656; 1245193; 1769482; 2359307; 2424844; 2883597; 3473422; 196612; 524303; 1507344; 1966097; 2686994; 262152; 196608; 458771; 720899; 1245193; 2359307; 2424844; 2883597; 3473428; 327684; 524303; 1507349; 1966097; 2686994; 589827; 786454; 1572887; 3080216; 655366; 196608; 720921; 2359307; 2424844; 2883597; 3473428; 720899; 786454; 1572890; 3080216; 917524; 65563; 131100; 196608; 393245; 458754; 655390; 720899; 983046; 1114119; 1179656; 1245193; 1310751; 1376288; 2162721; 2359307; 2424866; 2752547; 2883597; 3407908; 3473428; 983043; 37; 1638438; 1900583; 1048592; 65576; 196608; 393245; 458754; 720899; 983046; 1114119; 1179656; 1245193; 2162721; 2359307; 2424866; 2752547; 2883597; 3407908; 3473428; 1114115; 37; 1638441; 1900583; 1310727; 262186; 2097195; 2228268; 2293805; 2490414; 2555951; 2818096; 1376268; 196608; 393265; 458754; 720899; 983046; 1114119; 1179656; 1245193; 2359307; 2424844; 2883597; 3473428; 1507329; 3342386; 1572876; 196608; 393267; 458754; 720899; 983046; 1114119; 1179656; 1245193; 2359307; 2424844; 2883597; 3473428; 1638401; 2031668; 1703948; 196608; 393269; 458754; 720899; 983046; 1114119; 1179656; 1245193; 2359307; 2424844; 2883597; 3473428; 1835012; 1048630; 1441847; 2621496; 3014713; 1900554; 196608; 458754; 720899; 983098; 1179656; 1245193; 2359307; 2424844; 2883597; 3473428; 1966084; 1048630; 1441851; 2621496; 3014713; 2359308; 196608; 393276; 458754; 720899; 983046; 1114119; 1179656; 1245193; 2359307; 2424844; 2883597; 3473428; 2424833; 3145789; 3407892; 65563; 131134; 196608; 393279; 458754; 655390; 720899; 983046; 1114119; 1179656; 1245193; 1310751; 1376288; 2162721; 2359307; 2424866; 2752547; 2883597; 3407908; 3473428; 3473409; 3145792; 3604500; 65563; 131137; 196608; 393245; 458754; 655390; 720899; 983046; 1114119; 1179656; 1245193; 1310751; 1376288; 2162721; 2359307; 2424866; 2752547; 2883597; 3407908; 3473428; 3735555; 589890; 1704003; 2949188; 3801106; 65563; 196608; 393245; 458754; 655429; 720899; 983046; 1114119; 1179656; 1245193; 1376288; 2162721; 2359307; 2424866; 2752547; 2883597; 3407908; 3473428; 3866627; 589890; 1704006; 2949188; 4390920; 262186; 2097195; 2228268; 2293805; 2490414; 2555951; 2818096; 3145789; 4521985; 3276871; 4587534; 196608; 393217; 458754; 720899; 917509; 983046; 1114119; 1179656; 1245193; 1769544; 2359307; 2424844; 2883597; 3473422; 4718593; 2097225; 4784140; 196608; 393290; 458754; 720899; 983046; 1114119; 1179656; 1245193; 2359307; 2424844; 2883597; 3473428|]
let gotos = Array.zeroCreate 76
for i = 0 to 75 do
        gotos.[i] <- Array.zeroCreate 54
cur <- 0
while cur < small_gotos.Length do
    let i = small_gotos.[cur] >>> 16
    let length = small_gotos.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_gotos.[cur + k] >>> 16
        let x = small_gotos.[cur + k] &&& 65535
        gotos.[i].[j] <- lists_gotos.[x]
    cur <- cur + length
let private lists_reduces = [|[|26,1|]; [|5,1|]; [|13,1|]; [|15,2|]; [|15,3|]; [|17,1|]; [|16,1|]; [|19,1|]; [|21,2|]; [|21,3|]; [|22,1|]; [|34,1|]; [|36,2|]; [|36,3|]; [|37,1|]; [|38,3|]; [|27,6|]; [|7,1|]; [|9,2|]; [|9,3|]; [|11,1|]; [|10,1|]; [|12,1|]; [|18,1|]; [|25,3|]; [|23,1|]; [|24,1|]; [|7,2|]; [|6,1|]; [|19,2|]; [|13,2|]; [|43,1|]; [|46,1|]; [|45,1|]; [|48,1|]; [|47,1|]; [|44,1|]; [|41,1|]; [|39,3|]; [|42,2|]; [|29,1|]; [|31,2|]; [|31,3|]; [|32,1|]; [|33,1|]; [|34,2|]; [|29,2|]; [|28,1|]; [|40,1|]; [|2,2|]; [|3,3|]; [|4,3|]; [|0,1|]|]
let private small_reduces =
        [|65553; 1900544; 1966080; 2031616; 2097152; 2228224; 2293760; 2490368; 2555904; 2621440; 2686976; 2818048; 2949120; 3014656; 3080192; 3145728; 3276800; 3342336; 131073; 3276801; 196625; 1900546; 1966082; 2031618; 2097154; 2228226; 2293762; 2490370; 2555906; 2621442; 2686978; 2818050; 2949122; 3014658; 3080194; 3145730; 3276802; 3342338; 327697; 1900547; 1966083; 2031619; 2097155; 2228227; 2293763; 2490371; 2555907; 2621443; 2686979; 2818051; 2949123; 3014659; 3080195; 3145731; 3276803; 3342339; 393233; 1900548; 1966084; 2031620; 2097156; 2228228; 2293764; 2490372; 2555908; 2621444; 2686980; 2818052; 2949124; 3014660; 3080196; 3145732; 3276804; 3342340; 458756; 2359301; 2424837; 2883589; 3473413; 524292; 2359302; 2424838; 2883590; 3473414; 589841; 1900551; 1966087; 2031623; 2097159; 2228231; 2293767; 2490375; 2555911; 2621447; 2686983; 2818055; 2949127; 3014663; 3080199; 3145735; 3276807; 3342343; 720913; 1900552; 1966088; 2031624; 2097160; 2228232; 2293768; 2490376; 2555912; 2621448; 2686984; 2818056; 2949128; 3014664; 3080200; 3145736; 3276808; 3342344; 786449; 1900553; 1966089; 2031625; 2097161; 2228233; 2293769; 2490377; 2555913; 2621449; 2686985; 2818057; 2949129; 3014665; 3080201; 3145737; 3276809; 3342345; 851972; 2359306; 2424842; 2883594; 3473418; 983044; 1900555; 2949131; 3145739; 3342347; 1114116; 1900556; 2949132; 3145740; 3342348; 1179652; 1900557; 2949133; 3145741; 3342349; 1245191; 2162702; 2359310; 2424846; 2752526; 2883598; 3407886; 3473422; 1441796; 1900559; 2949135; 3145743; 3342351; 1769489; 1900560; 1966096; 2031632; 2097168; 2228240; 2293776; 2490384; 2555920; 2621456; 2686992; 2818064; 2949136; 3014672; 3080208; 3145744; 3276816; 3342352; 1835025; 1900561; 1966097; 2031633; 2097169; 2228241; 2293777; 2490385; 2555921; 2621457; 2686993; 2818065; 2949137; 3014673; 3080209; 3145745; 3276817; 3342353; 1966097; 1900562; 1966098; 2031634; 2097170; 2228242; 2293778; 2490386; 2555922; 2621458; 2686994; 2818066; 2949138; 3014674; 3080210; 3145746; 3276818; 3342354; 2031633; 1900563; 1966099; 2031635; 2097171; 2228243; 2293779; 2490387; 2555923; 2621459; 2686995; 2818067; 2949139; 3014675; 3080211; 3145747; 3276819; 3342355; 2097156; 2359316; 2424852; 2883604; 3473428; 2162692; 2359317; 2424853; 2883605; 3473429; 2228241; 1900566; 1966102; 2031638; 2097174; 2228246; 2293782; 2490390; 2555926; 2621462; 2686998; 2818070; 2949142; 3014678; 3080214; 3145750; 3276822; 3342358; 2293777; 1900567; 1966103; 2031639; 2097175; 2228247; 2293783; 2490391; 2555927; 2621463; 2686999; 2818071; 2949143; 3014679; 3080215; 3145751; 3276823; 3342359; 2490385; 1900568; 1966104; 2031640; 2097176; 2228248; 2293784; 2490392; 2555928; 2621464; 2687000; 2818072; 2949144; 3014680; 3080216; 3145752; 3276824; 3342360; 2555921; 1900569; 1966105; 2031641; 2097177; 2228249; 2293785; 2490393; 2555929; 2621465; 2687001; 2818073; 2949145; 3014681; 3080217; 3145753; 3276825; 3342361; 2621457; 1900570; 1966106; 2031642; 2097178; 2228250; 2293786; 2490394; 2555930; 2621466; 2687002; 2818074; 2949146; 3014682; 3080218; 3145754; 3276826; 3342362; 2686993; 1900571; 1966107; 2031643; 2097179; 2228251; 2293787; 2490395; 2555931; 2621467; 2687003; 2818075; 2949147; 3014683; 3080219; 3145755; 3276827; 3342363; 2752529; 1900572; 1966108; 2031644; 2097180; 2228252; 2293788; 2490396; 2555932; 2621468; 2687004; 2818076; 2949148; 3014684; 3080220; 3145756; 3276828; 3342364; 2818065; 1900573; 1966109; 2031645; 2097181; 2228253; 2293789; 2490397; 2555933; 2621469; 2687005; 2818077; 2949149; 3014685; 3080221; 3145757; 3276829; 3342365; 2883601; 1900574; 1966110; 2031646; 2097182; 2228254; 2293790; 2490398; 2555934; 2621470; 2687006; 2818078; 2949150; 3014686; 3080222; 3145758; 3276830; 3342366; 2949124; 2359327; 2424863; 2883615; 3473439; 3014660; 2359328; 2424864; 2883616; 3473440; 3080196; 2359329; 2424865; 2883617; 3473441; 3145732; 2359330; 2424866; 2883618; 3473442; 3211268; 2359331; 2424867; 2883619; 3473443; 3276804; 2359332; 2424868; 2883620; 3473444; 3342340; 1900581; 2949157; 3145765; 3342373; 3538948; 1900582; 2949158; 3145766; 3342374; 3670020; 1900583; 2949159; 3145767; 3342375; 3735556; 1900584; 2949160; 3145768; 3342376; 3866628; 1900585; 2949161; 3145769; 3342377; 3932164; 1900586; 2949162; 3145770; 3342378; 3997703; 2162731; 2359339; 2424875; 2752555; 2883627; 3407915; 3473451; 4063236; 1900588; 2949164; 3145772; 3342380; 4128772; 1900589; 2949165; 3145773; 3342381; 4194308; 1900590; 2949166; 3145774; 3342382; 4259844; 1900591; 2949167; 3145775; 3342383; 4325380; 1900592; 2949168; 3145776; 3342384; 4587521; 3211313; 4653057; 3211314; 4718598; 1966106; 2621466; 2687002; 3014682; 3080218; 3276826; 4849665; 3276851; 4915201; 3211316|]
let reduces = Array.zeroCreate 76
for i = 0 to 75 do
        reduces.[i] <- Array.zeroCreate 54
cur <- 0
while cur < small_reduces.Length do
    let i = small_reduces.[cur] >>> 16
    let length = small_reduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_reduces.[cur + k] >>> 16
        let x = small_reduces.[cur + k] &&& 65535
        reduces.[i].[j] <- lists_reduces.[x]
    cur <- cur + length
let private lists_zeroReduces = [|[|14|]; [|20|]; [|35|]; [|8|]; [|30|]|]
let private small_zeroReduces =
        [|196625; 1900544; 1966080; 2031616; 2097152; 2228224; 2293760; 2490368; 2555904; 2621440; 2686976; 2818048; 2949120; 3014656; 3080192; 3145728; 3276800; 3342336; 327697; 1900544; 1966080; 2031616; 2097152; 2228224; 2293760; 2490368; 2555904; 2621440; 2686976; 2818048; 2949120; 3014656; 3080192; 3145728; 3276800; 3342336; 589841; 1900545; 1966081; 2031617; 2097153; 2228225; 2293761; 2490369; 2555905; 2621441; 2686977; 2818049; 2949121; 3014657; 3080193; 3145729; 3276801; 3342337; 720913; 1900545; 1966081; 2031617; 2097153; 2228225; 2293761; 2490369; 2555905; 2621441; 2686977; 2818049; 2949121; 3014657; 3080193; 3145729; 3276801; 3342337; 983044; 1900546; 2949122; 3145730; 3342338; 1114116; 1900546; 2949122; 3145730; 3342338; 1835025; 1900547; 1966083; 2031619; 2097155; 2228227; 2293763; 2490371; 2555907; 2621443; 2686979; 2818051; 2949123; 3014659; 3080195; 3145731; 3276803; 3342339; 1966097; 1900547; 1966083; 2031619; 2097155; 2228227; 2293763; 2490371; 2555907; 2621443; 2686979; 2818051; 2949123; 3014659; 3080195; 3145731; 3276803; 3342339; 3735556; 1900548; 2949124; 3145732; 3342340; 3866628; 1900548; 2949124; 3145732; 3342340|]
let zeroReduces = Array.zeroCreate 76
for i = 0 to 75 do
        zeroReduces.[i] <- Array.zeroCreate 54
cur <- 0
while cur < small_zeroReduces.Length do
    let i = small_zeroReduces.[cur] >>> 16
    let length = small_zeroReduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_zeroReduces.[cur + k] >>> 16
        let x = small_zeroReduces.[cur + k] &&& 65535
        zeroReduces.[i].[j] <- lists_zeroReduces.[x]
    cur <- cur + length
let private small_acc = [68]
let private accStates = Array.zeroCreate 76
for i = 0 to 75 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 49
let errorIndex = 5
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; new Tree<_>(null,(new AST(new Family(49, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,(new AST(new Family(8, new Nodes([||])), null)), null); new Tree<_>(null,(new AST(new Family(14, new Nodes([||])), null)), null); new Tree<_>(null,(new AST(new Family(20, new Nodes([||])), null)), null); new Tree<_>(null,(new AST(new Family(35, new Nodes([||])), null)), null); new Tree<_>(null,(new AST(new Family(30, new Nodes([||])), null)), null); null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; new Tree<_>(null,(new AST(new Family(49, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,(new AST(new Family(8, new Nodes([||])), null)), null); new Tree<_>(null,(new AST(new Family(14, new Nodes([||])), null)), null); new Tree<_>(null,(new AST(new Family(20, new Nodes([||])), null)), null); new Tree<_>(null,(new AST(new Family(35, new Nodes([||])), null)), null); new Tree<_>(null,(new AST(new Family(30, new Nodes([||])), null)), null); null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_and * '_rnglr_type_andExpr * '_rnglr_type_boolCond * '_rnglr_type_boolExpr * '_rnglr_type_compOp * '_rnglr_type_error * '_rnglr_type_expr * '_rnglr_type_factor * '_rnglr_type_factorOp * '_rnglr_type_or * '_rnglr_type_orExpr * '_rnglr_type_powExpr * '_rnglr_type_powOp * '_rnglr_type_program * '_rnglr_type_stmt * '_rnglr_type_term * '_rnglr_type_termOp * '_rnglr_type_yard_rule_binExpr_1 * '_rnglr_type_yard_rule_binExpr_3 * '_rnglr_type_yard_rule_binExpr_5 * '_rnglr_type_yard_rule_boolCondMeta_7 * '_rnglr_type_yard_rule_boolCondMeta_9 * '_rnglr_type_yard_rule_yard_many_1_2 * '_rnglr_type_yard_rule_yard_many_1_4 * '_rnglr_type_yard_rule_yard_many_1_6 * '_rnglr_type_yard_rule_yard_many_2_10 * '_rnglr_type_yard_rule_yard_many_2_8 * '_rnglr_type_yard_some_1 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_some_1)  |> List.iter (fun (stmts) -> 
              _rnglr_cycle_res := (
                
# 19 "BoolCalc.yrd"
                                               
                let code = List.map (fun (st,semi) -> st) stmts
                (code, vars, returnVal)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 19 "BoolCalc.yrd"
               : '_rnglr_type_program) 
# 280 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_program) 
            )
# 19 "BoolCalc.yrd"
               : '_rnglr_type_yard_start_rule) 
# 290 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt)  |> List.iter (fun (S1) -> 
                (match ((unbox _rnglr_children.[1]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwithf "SEMI expected, but %A found" a )
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 19 "BoolCalc.yrd"
                                      S1, S2
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 19 "BoolCalc.yrd"
                                  [yard_elem]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 19 "BoolCalc.yrd"
               : '_rnglr_type_yard_some_1) 
# 320 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt)  |> List.iter (fun (S1) -> 
                (match ((unbox _rnglr_children.[1]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwithf "SEMI expected, but %A found" a )
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 19 "BoolCalc.yrd"
                                      S1, S2
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_some_1) 
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 19 "BoolCalc.yrd"
                                    yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 19 "BoolCalc.yrd"
               : '_rnglr_type_yard_some_1) 
# 352 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with VAR _rnglr_val -> [_rnglr_val] | a -> failwithf "VAR expected, but %A found" a )
             |> List.iter (fun (v) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with EQ _rnglr_val -> [_rnglr_val] | a -> failwithf "EQ expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_0) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_expr) 
                 |> List.iter (fun (e) -> 
                  _rnglr_cycle_res := (
                    
# 25 "BoolCalc.yrd"
                                        
                       let result = getValue(e)
                       if (vars.ContainsKey v) then
                         vars.[v] <- result
                       else
                         vars.Add(v, result)
                       returnVal <- result
                       EqStmt(v, e)
                       
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 24 "BoolCalc.yrd"
               : '_rnglr_type_stmt) 
# 384 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_expr)  |> List.iter (fun (e) -> 
              _rnglr_cycle_res := (
                
# 35 "BoolCalc.yrd"
                             
                   returnVal <- getValue(e)
                   SingleExpr(e)
                   
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 24 "BoolCalc.yrd"
               : '_rnglr_type_stmt) 
# 406 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_1)  |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 44 "BoolCalc.yrd"
                      S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 44 "BoolCalc.yrd"
               : '_rnglr_type_expr) 
# 425 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_term)  |> List.iter (fun (h) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_2) h
               |> List.iter (fun (tl) -> 
                _rnglr_cycle_res := (
                  
# 40 "BoolCalc.yrd"
                                                                         
                     List.fold calcFunc h tl
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 40 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_binExpr_1) 
# 448 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun h ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 40 "BoolCalc.yrd"
                                                    []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 40 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_2) 
# 466 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun h ->
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_termOp) 
               |> List.iter (fun (S1) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_term) 
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 44 "BoolCalc.yrd"
                                       S1, S2
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_2) h
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 40 "BoolCalc.yrd"
                                                        yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 40 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_2) 
# 499 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PLUS _rnglr_val -> [_rnglr_val] | a -> failwithf "PLUS expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 46 "BoolCalc.yrd"
                              Plus
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 46 "BoolCalc.yrd"
               : '_rnglr_type_termOp) 
# 519 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MINUS _rnglr_val -> [_rnglr_val] | a -> failwithf "MINUS expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 46 "BoolCalc.yrd"
                                             Minus
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 46 "BoolCalc.yrd"
               : '_rnglr_type_termOp) 
# 539 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_3)  |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 48 "BoolCalc.yrd"
                      S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 48 "BoolCalc.yrd"
               : '_rnglr_type_term) 
# 558 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_factor)  |> List.iter (fun (h) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_4) h
               |> List.iter (fun (tl) -> 
                _rnglr_cycle_res := (
                  
# 40 "BoolCalc.yrd"
                                                                         
                     List.fold calcFunc h tl
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 40 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_binExpr_3) 
# 581 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun h ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 40 "BoolCalc.yrd"
                                                    []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 40 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_4) 
# 599 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun h ->
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_factorOp) 
               |> List.iter (fun (S1) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_factor) 
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 48 "BoolCalc.yrd"
                                         S1, S2
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_4) h
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 40 "BoolCalc.yrd"
                                                        yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 40 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_4) 
# 632 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MULT _rnglr_val -> [_rnglr_val] | a -> failwithf "MULT expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 50 "BoolCalc.yrd"
                                Mult
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 50 "BoolCalc.yrd"
               : '_rnglr_type_factorOp) 
# 652 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DIV _rnglr_val -> [_rnglr_val] | a -> failwithf "DIV expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 50 "BoolCalc.yrd"
                                             Div
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 50 "BoolCalc.yrd"
               : '_rnglr_type_factorOp) 
# 672 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_5)  |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 52 "BoolCalc.yrd"
                        S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 52 "BoolCalc.yrd"
               : '_rnglr_type_factor) 
# 691 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_powExpr)  |> List.iter (fun (h) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_6) h
               |> List.iter (fun (tl) -> 
                _rnglr_cycle_res := (
                  
# 40 "BoolCalc.yrd"
                                                                         
                     List.fold calcFunc h tl
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 40 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_binExpr_5) 
# 714 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun h ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 40 "BoolCalc.yrd"
                                                    []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 40 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_6) 
# 732 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun h ->
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_powOp) 
               |> List.iter (fun (S1) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_powExpr) 
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 52 "BoolCalc.yrd"
                                            S1, S2
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_6) h
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 40 "BoolCalc.yrd"
                                                        yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 40 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_6) 
# 765 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with POW _rnglr_val -> [_rnglr_val] | a -> failwithf "POW expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 54 "BoolCalc.yrd"
                            Pow
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 54 "BoolCalc.yrd"
               : '_rnglr_type_powOp) 
# 785 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NUMBER _rnglr_val -> [_rnglr_val] | a -> failwithf "NUMBER expected, but %A found" a )
             |> List.iter (fun (n) -> 
              _rnglr_cycle_res := (
                
# 57 "BoolCalc.yrd"
                             Num (float n)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 56 "BoolCalc.yrd"
               : '_rnglr_type_powExpr) 
# 805 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with VAR _rnglr_val -> [_rnglr_val] | a -> failwithf "VAR expected, but %A found" a )
             |> List.iter (fun (v) -> 
              _rnglr_cycle_res := (
                
# 58 "BoolCalc.yrd"
                            EVar(v)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 56 "BoolCalc.yrd"
               : '_rnglr_type_powExpr) 
# 825 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LBRACE _rnglr_val -> [_rnglr_val] | a -> failwithf "LBRACE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_expr) 
               |> List.iter (fun (e) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with RBRACE _rnglr_val -> [_rnglr_val] | a -> failwithf "RBRACE expected, but %A found" a )
                 |> List.iter (fun (_rnglr_var_1) -> 
                  _rnglr_cycle_res := (
                    
# 59 "BoolCalc.yrd"
                                               e
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 56 "BoolCalc.yrd"
               : '_rnglr_type_powExpr) 
# 849 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_boolExpr)  |> List.iter (fun (e) -> 
              _rnglr_cycle_res := (
                
# 60 "BoolCalc.yrd"
                                 e
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 56 "BoolCalc.yrd"
               : '_rnglr_type_powExpr) 
# 868 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with IF _rnglr_val -> [_rnglr_val] | a -> failwithf "IF expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_boolCond) 
               |> List.iter (fun (c) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with THEN _rnglr_val -> [_rnglr_val] | a -> failwithf "THEN expected, but %A found" a )
                 |> List.iter (fun (_rnglr_var_1) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_expr) 
                   |> List.iter (fun (e1) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with ELSE _rnglr_val -> [_rnglr_val] | a -> failwithf "ELSE expected, but %A found" a )
                     |> List.iter (fun (_rnglr_var_2) -> 
                      ((unbox _rnglr_children.[5]) : '_rnglr_type_expr) 
                       |> List.iter (fun (e2) -> 
                        _rnglr_cycle_res := (
                          
# 64 "BoolCalc.yrd"
                                                                             
                             IfThenElse(c,e1,e2)
                          
                            )::!_rnglr_cycle_res ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 64 "BoolCalc.yrd"
               : '_rnglr_type_boolExpr) 
# 900 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_boolCondMeta_7) 
             |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 72 "BoolCalc.yrd"
                          S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 72 "BoolCalc.yrd"
               : '_rnglr_type_boolCond) 
# 920 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_orExpr)  |> List.iter (fun (h) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_2_8) h
               |> List.iter (fun (tl) -> 
                _rnglr_cycle_res := (
                  
# 68 "BoolCalc.yrd"
                                                                                
                     List.fold calcLogBool h tl
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 68 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_boolCondMeta_7) 
# 943 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun h ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 40 "BoolCalc.yrd"
                                                    []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 68 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_yard_many_2_8) 
# 961 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun h ->
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_or) 
               |> List.iter (fun (S1) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_orExpr) 
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 72 "BoolCalc.yrd"
                                                  S1, S2
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_2_8) h
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 68 "BoolCalc.yrd"
                                                              yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 68 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_yard_many_2_8) 
# 994 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with OR _rnglr_val -> [_rnglr_val] | a -> failwithf "OR expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 74 "BoolCalc.yrd"
                        Or
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 74 "BoolCalc.yrd"
               : '_rnglr_type_or) 
# 1014 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_boolCondMeta_9) 
             |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 76 "BoolCalc.yrd"
                        S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 76 "BoolCalc.yrd"
               : '_rnglr_type_orExpr) 
# 1034 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_andExpr)  |> List.iter (fun (h) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_2_10) h
               |> List.iter (fun (tl) -> 
                _rnglr_cycle_res := (
                  
# 68 "BoolCalc.yrd"
                                                                                
                     List.fold calcLogBool h tl
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 68 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_boolCondMeta_9) 
# 1057 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun h ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 40 "BoolCalc.yrd"
                                                    []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 68 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_yard_many_2_10) 
# 1075 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun h ->
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_and) 
               |> List.iter (fun (S1) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_andExpr) 
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 76 "BoolCalc.yrd"
                                                 S1, S2
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_2_10) h
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 68 "BoolCalc.yrd"
                                                              yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 68 "BoolCalc.yrd"
               : '_rnglr_type_yard_rule_yard_many_2_10) 
# 1108 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with AND _rnglr_val -> [_rnglr_val] | a -> failwithf "AND expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 78 "BoolCalc.yrd"
                          And
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 78 "BoolCalc.yrd"
               : '_rnglr_type_and) 
# 1128 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_expr)  |> List.iter (fun (e1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_compOp) 
               |> List.iter (fun (o) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_expr) 
                 |> List.iter (fun (e2) -> 
                  _rnglr_cycle_res := (
                    
# 81 "BoolCalc.yrd"
                                                 calcCompBool e1 o e2 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 80 "BoolCalc.yrd"
               : '_rnglr_type_andExpr) 
# 1151 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LBRACE _rnglr_val -> [_rnglr_val] | a -> failwithf "LBRACE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_boolCond) 
               |> List.iter (fun (e) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with RBRACE _rnglr_val -> [_rnglr_val] | a -> failwithf "RBRACE expected, but %A found" a )
                 |> List.iter (fun (_rnglr_var_1) -> 
                  _rnglr_cycle_res := (
                    
# 82 "BoolCalc.yrd"
                                                    e 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 80 "BoolCalc.yrd"
               : '_rnglr_type_andExpr) 
# 1175 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with TRUE _rnglr_val -> [_rnglr_val] | a -> failwithf "TRUE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 83 "BoolCalc.yrd"
                           BoolConst true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 80 "BoolCalc.yrd"
               : '_rnglr_type_andExpr) 
# 1195 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with FALSE _rnglr_val -> [_rnglr_val] | a -> failwithf "FALSE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 84 "BoolCalc.yrd"
                            BoolConst false
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 80 "BoolCalc.yrd"
               : '_rnglr_type_andExpr) 
# 1215 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NOT _rnglr_val -> [_rnglr_val] | a -> failwithf "NOT expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_boolCond) 
               |> List.iter (fun (b) -> 
                _rnglr_cycle_res := (
                  
# 85 "BoolCalc.yrd"
                                       calcNotBool b
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 80 "BoolCalc.yrd"
               : '_rnglr_type_andExpr) 
# 1237 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EQ _rnglr_val -> [_rnglr_val] | a -> failwithf "EQ expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 87 "BoolCalc.yrd"
                            Equal
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 87 "BoolCalc.yrd"
               : '_rnglr_type_compOp) 
# 1257 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NOTEQ _rnglr_val -> [_rnglr_val] | a -> failwithf "NOTEQ expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 87 "BoolCalc.yrd"
                                            NotEqual
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 87 "BoolCalc.yrd"
               : '_rnglr_type_compOp) 
# 1277 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with GR _rnglr_val -> [_rnglr_val] | a -> failwithf "GR expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 87 "BoolCalc.yrd"
                                                            Gr
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 87 "BoolCalc.yrd"
               : '_rnglr_type_compOp) 
# 1297 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with GE _rnglr_val -> [_rnglr_val] | a -> failwithf "GE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 87 "BoolCalc.yrd"
                                                                      Ge
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 87 "BoolCalc.yrd"
               : '_rnglr_type_compOp) 
# 1317 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LS _rnglr_val -> [_rnglr_val] | a -> failwithf "LS expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 87 "BoolCalc.yrd"
                                                                                Ls
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 87 "BoolCalc.yrd"
               : '_rnglr_type_compOp) 
# 1337 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LE _rnglr_val -> [_rnglr_val] | a -> failwithf "LE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 87 "BoolCalc.yrd"
                                                                                          Le
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 87 "BoolCalc.yrd"
               : '_rnglr_type_compOp) 
# 1357 "BoolCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              parserRange
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_error) 
# 1375 "BoolCalc.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_and)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_andExpr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_boolCond)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_boolExpr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_compOp)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_expr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_factor)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_factorOp)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_or)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_orExpr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_powExpr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_powOp)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_program)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_term)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_termOp)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_3)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_5)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_boolCondMeta_7)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_boolCondMeta_9)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun h ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_2)  h ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun h ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_4)  h ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun h ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_6)  h ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun h ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_2_10)  h ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun h ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_2_8)  h ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_some_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
