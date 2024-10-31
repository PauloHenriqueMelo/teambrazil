



#' This function processes key SIH variables, converting them to appropriate formats and labels, if the variables exist in the data.
#'
#' @param data A \code{data.frame} containing SIH data.
#' @return A \code{data.frame} with the processed data.
#' @examples
#' # Example usage:
#' process_rd(sih_rd_sample)
#' @import dplyr
#' @importFrom dplyr mutate
#' @export




process_rd <- function(data) {

  utils::data("icd_codes", package = "teambrazil")
  utils::data("proc_rea", package = "teambrazil")
  utils::data("ibge", package = "teambrazil")


  # List of variables to keep if they exist in the dataset
  variables_to_keep <- c(
    "ANO_CMPT", "MES_CMPT", "ESPEC", "N_AIH", "IDENT",
    "MUNIC_RES", "NASC", "SEXO", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT",
    "VAL_UTI", "US_TOT", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN", "DIAGSEC1", "TPDISEC1", "DIAGSEC2",
    "TPDISEC2","DIAGSEC3", "TPDISEC3", "DIAGSEC4",
    "TPDISEC4", "DIAGSEC5","TPDISEC5",
    "COBRANCA", "NATUREZA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM",
    "MORTE", "CAR_INT", "CNES","CID_MORTE", "ETNIA",
    "COMPLEX", "FINANC", "RACA_COR"
  )

  # Keep only the variables that are present in the dataset
  available_vars <- intersect(variables_to_keep, names(data))
  data <- dplyr::select(data, all_of(available_vars))


  if ("VAL_UTI" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(ICU_Service_Value = as.numeric(VAL_UTI)) %>%
      dplyr::select(-VAL_UTI)
  }


  if ("ETNIA" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Indigenous_Ethnicity = as.character(ETNIA)) %>%
      dplyr::mutate(Indigenous_Ethnicity = dplyr::case_when(
        Indigenous_Ethnicity == "0001" ~ "ACONA",
        Indigenous_Ethnicity == "0002" ~ "AIKANA",
        Indigenous_Ethnicity == "0003" ~ "AJURU",
        Indigenous_Ethnicity == "0004" ~ "AKUNTSU",
        Indigenous_Ethnicity == "0005" ~ "AMANAYE",
        Indigenous_Ethnicity == "0006" ~ "AMONDAWA",
        Indigenous_Ethnicity == "0007" ~ "ANAMBE",
        Indigenous_Ethnicity == "0008" ~ "APARAI",
        Indigenous_Ethnicity == "0009" ~ "APIAKA",
        Indigenous_Ethnicity == "0010" ~ "APINAYE",
        Indigenous_Ethnicity == "0011" ~ "APURINA",
        Indigenous_Ethnicity == "0012" ~ "ARANA",
        Indigenous_Ethnicity == "0013" ~ "ARAPASO",
        Indigenous_Ethnicity == "0014" ~ "ARARA DE RONDONIA",
        Indigenous_Ethnicity == "0015" ~ "ARARA DO ACRE",
        Indigenous_Ethnicity == "0016" ~ "ARARA DO ARIPUANA",
        Indigenous_Ethnicity == "0017" ~ "ARARA DO PARA",
        Indigenous_Ethnicity == "0018" ~ "ARAWETE",
        Indigenous_Ethnicity == "0019" ~ "ARIKAPU",
        Indigenous_Ethnicity == "0020" ~ "ARIKEM",
        Indigenous_Ethnicity == "0021" ~ "ARIKOSE",
        Indigenous_Ethnicity == "0022" ~ "ARUA",
        Indigenous_Ethnicity == "0023" ~ "ARUAK",
        Indigenous_Ethnicity == "0024" ~ "ASHANINKA",
        Indigenous_Ethnicity == "0025" ~ "ASURINI DO TOCANTINS",
        Indigenous_Ethnicity == "0026" ~ "ASURINI DO XINGU",
        Indigenous_Ethnicity == "0027" ~ "ATIKUM",
        Indigenous_Ethnicity == "0028" ~ "AVA CANOEIRO",
        Indigenous_Ethnicity == "0029" ~ "AWETI",
        Indigenous_Ethnicity == "0030" ~ "BAKAIRI",
        Indigenous_Ethnicity == "0031" ~ "BANAWA YAFI",
        Indigenous_Ethnicity == "0032" ~ "BANIWA",
        Indigenous_Ethnicity == "0033" ~ "BARA",
        Indigenous_Ethnicity == "0034" ~ "BARASANA",
        Indigenous_Ethnicity == "0035" ~ "BARE",
        Indigenous_Ethnicity == "0036" ~ "BORORO",
        Indigenous_Ethnicity == "0037" ~ "BOTOCUDO",
        Indigenous_Ethnicity == "0038" ~ "CANOE",
        Indigenous_Ethnicity == "0039" ~ "CASSUPA",
        Indigenous_Ethnicity == "0040" ~ "CHAMACOCO",
        Indigenous_Ethnicity == "0041" ~ "CHIQUITANO",
        Indigenous_Ethnicity == "0042" ~ "CIKIYANA",
        Indigenous_Ethnicity == "0043" ~ "CINTA LARGA",
        Indigenous_Ethnicity == "0044" ~ "COLUMBIARA",
        Indigenous_Ethnicity == "0045" ~ "DENI",
        Indigenous_Ethnicity == "0046" ~ "DESANA",
        Indigenous_Ethnicity == "0047" ~ "DIAHUI",
        Indigenous_Ethnicity == "0048" ~ "ENAWENE NAWE",
        Indigenous_Ethnicity == "0049" ~ "FULNI O",
        Indigenous_Ethnicity == "0050" ~ "GALIBI",
        Indigenous_Ethnicity == "0051" ~ "GALIBI MARWORNO",
        Indigenous_Ethnicity == "0052" ~ "GAVIAO DE RONDONIA",
        Indigenous_Ethnicity == "0053" ~ "GAVIAO KRIKATEJE",
        Indigenous_Ethnicity == "0054" ~ "GAVIAO PARKATEJE",
        Indigenous_Ethnicity == "0055" ~ "GAVIAO PUKOBIE",
        Indigenous_Ethnicity == "0056" ~ "GUAJA",
        Indigenous_Ethnicity == "0057" ~ "GUAJAJARA",
        Indigenous_Ethnicity == "0058" ~ "GUARANI KAIOWA",
        Indigenous_Ethnicity == "0059" ~ "GUARANI MBYA",
        Indigenous_Ethnicity == "0060" ~ "GUARANI NANDEVA",
        Indigenous_Ethnicity == "0061" ~ "GUATO",
        Indigenous_Ethnicity == "0062" ~ "HIMARIMA",
        Indigenous_Ethnicity == "0063" ~ "INGARIKO",
        Indigenous_Ethnicity == "0064" ~ "IRANXE",
        Indigenous_Ethnicity == "0065" ~ "ISSE",
        Indigenous_Ethnicity == "0066" ~ "JABOTI",
        Indigenous_Ethnicity == "0067" ~ "JAMAMADI",
        Indigenous_Ethnicity == "0068" ~ "JARAWARA",
        Indigenous_Ethnicity == "0069" ~ "JIRIPANCO",
        Indigenous_Ethnicity == "0070" ~ "JUMA",
        Indigenous_Ethnicity == "0071" ~ "JURUNA",
        Indigenous_Ethnicity == "0072" ~ "JURUTI",
        Indigenous_Ethnicity == "0073" ~ "KAAPOR",
        Indigenous_Ethnicity == "0074" ~ "KADIWEU",
        Indigenous_Ethnicity == "0075" ~ "KAIABI",
        Indigenous_Ethnicity == "0076" ~ "KAIMBE",
        Indigenous_Ethnicity == "0077" ~ "KAINGANG",
        Indigenous_Ethnicity == "0078" ~ "KAIXANA",
        Indigenous_Ethnicity == "0079" ~ "KALABASSA",
        Indigenous_Ethnicity == "0080" ~ "KALANCO",
        Indigenous_Ethnicity == "0081" ~ "KALAPALO",
        Indigenous_Ethnicity == "0082" ~ "KAMAYURA",
        Indigenous_Ethnicity == "0083" ~ "KAMBA",
        Indigenous_Ethnicity == "0084" ~ "KAMBEBA",
        Indigenous_Ethnicity == "0085" ~ "KAMBIWA",
        Indigenous_Ethnicity == "0086" ~ "KAMBIWA PIPIPA",
        Indigenous_Ethnicity == "0087" ~ "KAMPE",
        Indigenous_Ethnicity == "0088" ~ "KANAMANTI",
        Indigenous_Ethnicity == "0089" ~ "KANAMARI",
        Indigenous_Ethnicity == "0090" ~ "KANELA APANIEKRA",
        Indigenous_Ethnicity == "0091" ~ "KANELA RANKOKAMEKRA",
        Indigenous_Ethnicity == "0092" ~ "KANINDE",
        Indigenous_Ethnicity == "0093" ~ "KANOE",
        Indigenous_Ethnicity == "0094" ~ "KANTARURE",
        Indigenous_Ethnicity == "0095" ~ "KAPINAWA",
        Indigenous_Ethnicity == "0096" ~ "KARAJA",
        Indigenous_Ethnicity == "0097" ~ "KARAJA JAVAE",
        Indigenous_Ethnicity == "0098" ~ "KARAJA XAMBIOA",
        Indigenous_Ethnicity == "0099" ~ "KARAPANA",
        Indigenous_Ethnicity == "0100" ~ "KARAPOTO",
        Indigenous_Ethnicity == "0101" ~ "KARIPUNA",
        Indigenous_Ethnicity == "0102" ~ "KARIPUNA DO AMAPA",
        Indigenous_Ethnicity == "0103" ~ "KARIRI",
        Indigenous_Ethnicity == "0104" ~ "KARIRI XOCO",
        Indigenous_Ethnicity == "0105" ~ "KARITIANA",
        Indigenous_Ethnicity == "0106" ~ "KATAWIXI",
        Indigenous_Ethnicity == "0107" ~ "KATUENA",
        Indigenous_Ethnicity == "0108" ~ "KATUKINA",
        Indigenous_Ethnicity == "0109" ~ "KATUKINA DO ACRE",
        Indigenous_Ethnicity == "0110" ~ "KAXARARI",
        Indigenous_Ethnicity == "0111" ~ "KAXINAWA",
        Indigenous_Ethnicity == "0112" ~ "KAXIXO",
        Indigenous_Ethnicity == "0113" ~ "KAXUYANA",
        Indigenous_Ethnicity == "0114" ~ "KAYAPO",
        Indigenous_Ethnicity == "0115" ~ "KAYAPO KARARAO",
        Indigenous_Ethnicity == "0116" ~ "KAYAPO TXUKAHAMAE",
        Indigenous_Ethnicity == "0117" ~ "KAYAPO XICRIM",
        Indigenous_Ethnicity == "0118" ~ "KAYUISANA",
        Indigenous_Ethnicity == "0119" ~ "KINIKINAWA",
        Indigenous_Ethnicity == "0120" ~ "KIRIRI",
        Indigenous_Ethnicity == "0121" ~ "KOCAMA",
        Indigenous_Ethnicity == "0122" ~ "KOKUIREGATEJE",
        Indigenous_Ethnicity == "0123" ~ "KORUBO",
        Indigenous_Ethnicity == "0124" ~ "KRAHO",
        Indigenous_Ethnicity == "0125" ~ "KREJE",
        Indigenous_Ethnicity == "0126" ~ "KRENAK",
        Indigenous_Ethnicity == "0127" ~ "KRIKATI",
        Indigenous_Ethnicity == "0128" ~ "KUBEO",
        Indigenous_Ethnicity == "0129" ~ "KUIKURO",
        Indigenous_Ethnicity == "0130" ~ "KUJUBIM",
        Indigenous_Ethnicity == "0131" ~ "KULINA PANO",
        Indigenous_Ethnicity == "0132" ~ "KULINA MADIHA",
        Indigenous_Ethnicity == "0133" ~ "KURIPAKO",
        Indigenous_Ethnicity == "0134" ~ "KURUAIA",
        Indigenous_Ethnicity == "0135" ~ "KWAZA",
        Indigenous_Ethnicity == "0136" ~ "MACHINERI",
        Indigenous_Ethnicity == "0137" ~ "MACURAP",
        Indigenous_Ethnicity == "0138" ~ "MAKU DOW",
        Indigenous_Ethnicity == "0139" ~ "MAKU HUPDA",
        Indigenous_Ethnicity == "0140" ~ "MAKU NADEB",
        Indigenous_Ethnicity == "0141" ~ "MAKU YUHUPDE",
        Indigenous_Ethnicity == "0142" ~ "MAKUNA",
        Indigenous_Ethnicity == "0143" ~ "MAKUXI",
        Indigenous_Ethnicity == "0144" ~ "MARIMAM",
        Indigenous_Ethnicity == "0145" ~ "MARUBO",
        Indigenous_Ethnicity == "0146" ~ "MATIPU",
        Indigenous_Ethnicity == "0147" ~ "MATIS",
        Indigenous_Ethnicity == "0148" ~ "MATSE",
        Indigenous_Ethnicity == "0149" ~ "MAXAKALI",
        Indigenous_Ethnicity == "0150" ~ "MAYA",
        Indigenous_Ethnicity == "0151" ~ "MAYTAPU",
        Indigenous_Ethnicity == "0152" ~ "MEHINAKO",
        Indigenous_Ethnicity == "0153" ~ "MEKEN",
        Indigenous_Ethnicity == "0154" ~ "MENKY",
        Indigenous_Ethnicity == "0155" ~ "MIRANHA",
        Indigenous_Ethnicity == "0156" ~ "MIRITI TAPUIA",
        Indigenous_Ethnicity == "0157" ~ "MUNDURUKU",
        Indigenous_Ethnicity == "0158" ~ "MURA",
        Indigenous_Ethnicity == "0159" ~ "NAHUKWA",
        Indigenous_Ethnicity == "0160" ~ "NAMBIKWARA DO CAMPO",
        Indigenous_Ethnicity == "0161" ~ "NAMBIKWARA DO NORTE",
        Indigenous_Ethnicity == "0162" ~ "NAMBIKWARA DO SUL",
        Indigenous_Ethnicity == "0163" ~ "NARAVUTE",
        Indigenous_Ethnicity == "0164" ~ "NAWA",
        Indigenous_Ethnicity == "0165" ~ "NUKINI",
        Indigenous_Ethnicity == "0166" ~ "OFAIE",
        Indigenous_Ethnicity == "0167" ~ "ORO WIN",
        Indigenous_Ethnicity == "0168" ~ "PAIAKU",
        Indigenous_Ethnicity == "0169" ~ "PAKAA NOVA",
        Indigenous_Ethnicity == "0170" ~ "PALIKUR",
        Indigenous_Ethnicity == "0171" ~ "PANARA",
        Indigenous_Ethnicity == "0172" ~ "PANKARARE",
        Indigenous_Ethnicity == "0173" ~ "PANKARARU",
        Indigenous_Ethnicity == "0174" ~ "PANKARARU KALANKO",
        Indigenous_Ethnicity == "0175" ~ "PANKARARU KARUAZU",
        Indigenous_Ethnicity == "0176" ~ "PANKARU",
        Indigenous_Ethnicity == "0177" ~ "PARAKANA",
        Indigenous_Ethnicity == "0178" ~ "PARECI",
        Indigenous_Ethnicity == "0179" ~ "PARINTINTIN",
        Indigenous_Ethnicity == "0180" ~ "PATAMONA",
        Indigenous_Ethnicity == "0181" ~ "PATAXO",
        Indigenous_Ethnicity == "0182" ~ "PATAXO HAHAHAE",
        Indigenous_Ethnicity == "0183" ~ "PAUMARI",
        Indigenous_Ethnicity == "0184" ~ "PAUMELENHO",
        Indigenous_Ethnicity == "0185" ~ "PIRAHA",
        Indigenous_Ethnicity == "0186" ~ "PIRATUAPUIA",
        Indigenous_Ethnicity == "0187" ~ "PITAGUARI",
        Indigenous_Ethnicity == "0188" ~ "POTIGUARA",
        Indigenous_Ethnicity == "0189" ~ "POYANAWA",
        Indigenous_Ethnicity == "0190" ~ "RIKBAKTSA",
        Indigenous_Ethnicity == "0191" ~ "SAKURABIAT",
        Indigenous_Ethnicity == "0192" ~ "SATERE MAWE",
        Indigenous_Ethnicity == "0193" ~ "SHANENAWA",
        Indigenous_Ethnicity == "0194" ~ "SIRIANO",
        Indigenous_Ethnicity == "0195" ~ "SURIANA",
        Indigenous_Ethnicity == "0196" ~ "SURUI DE RONDONIA",
        Indigenous_Ethnicity == "0197" ~ "SURUI DO PARA",
        Indigenous_Ethnicity == "0198" ~ "SUYA",
        Indigenous_Ethnicity == "0199" ~ "TAPAYUNA",
        Indigenous_Ethnicity == "0200" ~ "TAPEBA",
        Indigenous_Ethnicity == "0201" ~ "TAPIRAPE",
        Indigenous_Ethnicity == "0202" ~ "TAPUIA",
        Indigenous_Ethnicity == "0203" ~ "TARIANO",
        Indigenous_Ethnicity == "0204" ~ "TAUREPANG",
        Indigenous_Ethnicity == "0205" ~ "TEMBE",
        Indigenous_Ethnicity == "0206" ~ "TENHARIM",
        Indigenous_Ethnicity == "0207" ~ "TERENA",
        Indigenous_Ethnicity == "0208" ~ "TICUNA",
        Indigenous_Ethnicity == "0209" ~ "TINGUI BOTO",
        Indigenous_Ethnicity == "0210" ~ "TIRIYO EWARHUYANA",
        Indigenous_Ethnicity == "0211" ~ "TIRIYO KAHYANA",
        Indigenous_Ethnicity == "0212" ~ "TIRIYO TSIKUYANA",
        Indigenous_Ethnicity == "0213" ~ "TORA",
        Indigenous_Ethnicity == "0214" ~ "TREMEMBE",
        Indigenous_Ethnicity == "0215" ~ "TRUKA",
        Indigenous_Ethnicity == "0216" ~ "TRUMAI",
        Indigenous_Ethnicity == "0217" ~ "TSOHOM DJAPA",
        Indigenous_Ethnicity == "0218" ~ "TUKANO",
        Indigenous_Ethnicity == "0219" ~ "TUMBALALA",
        Indigenous_Ethnicity == "0220" ~ "TUNAYANA",
        Indigenous_Ethnicity == "0221" ~ "TUPARI",
        Indigenous_Ethnicity == "0222" ~ "TUPINAMBA",
        Indigenous_Ethnicity == "0223" ~ "TUPINIQUIM",
        Indigenous_Ethnicity == "0224" ~ "TURIWARA",
        Indigenous_Ethnicity == "0225" ~ "TUXA",
        Indigenous_Ethnicity == "0226" ~ "TUYUKA",
        Indigenous_Ethnicity == "0227" ~ "TXIKAO",
        Indigenous_Ethnicity == "0228" ~ "UMUTINA",
        Indigenous_Ethnicity == "0229" ~ "URU EU WAU WAU",
        Indigenous_Ethnicity == "0230" ~ "WAI WAI HIXKARYANA",
        Indigenous_Ethnicity == "0231" ~ "WAI WAI KARAFAWYANA",
        Indigenous_Ethnicity == "0232" ~ "WAI WAI XEREU",
        Indigenous_Ethnicity == "0233" ~ "WAI WAI KATUENA",
        Indigenous_Ethnicity == "0234" ~ "WAI WAI MAWAYANA",
        Indigenous_Ethnicity == "0235" ~ "WAIAPI",
        Indigenous_Ethnicity == "0236" ~ "WAIMIRI ATROARI",
        Indigenous_Ethnicity == "0237" ~ "WANANO",
        Indigenous_Ethnicity == "0238" ~ "WAPIXANA",
        Indigenous_Ethnicity == "0239" ~ "WAREKENA",
        Indigenous_Ethnicity == "0240" ~ "WASSU",
        Indigenous_Ethnicity == "0241" ~ "WAURA",
        Indigenous_Ethnicity == "0242" ~ "WAYANA",
        Indigenous_Ethnicity == "0243" ~ "WITOTO",
        Indigenous_Ethnicity == "0244" ~ "XAKRIABA",
        Indigenous_Ethnicity == "0245" ~ "XAVANTE",
        Indigenous_Ethnicity == "0246" ~ "XERENTE",
        Indigenous_Ethnicity == "0247" ~ "XETA",
        Indigenous_Ethnicity == "0248" ~ "XIPAIA",
        Indigenous_Ethnicity == "0249" ~ "XOKLENG",
        Indigenous_Ethnicity == "0250" ~ "XOKO",
        Indigenous_Ethnicity == "0251" ~ "XUKURU",
        Indigenous_Ethnicity == "0252" ~ "XUKURU KARIRI",
        Indigenous_Ethnicity == "0253" ~ "YAIPIYANA",
        Indigenous_Ethnicity == "0254" ~ "YAMINAWA",
        Indigenous_Ethnicity == "0255" ~ "YANOMAMI NINAM",
        Indigenous_Ethnicity == "0256" ~ "YANOMAMI SANUMA",
        Indigenous_Ethnicity == "0257" ~ "YANOMAMI YANOMAM",
        Indigenous_Ethnicity == "0258" ~ "YAWALAPITI",
        Indigenous_Ethnicity == "0259" ~ "YAWANAWA",
        Indigenous_Ethnicity == "0260" ~ "YEKUANA",
        Indigenous_Ethnicity == "0261" ~ "YUDJA",
        Indigenous_Ethnicity == "0262" ~ "ZOE",
        Indigenous_Ethnicity == "0263" ~ "ZORO",
        Indigenous_Ethnicity == "0264" ~ "ZURUAHA",
        Indigenous_Ethnicity == "X265" ~ "AHANENAWA",
        Indigenous_Ethnicity == "X266" ~ "AICABA",
        Indigenous_Ethnicity == "X267" ~ "AIKANA KWASA",
        Indigenous_Ethnicity == "X268" ~ "AKUNTSU",
        Indigenous_Ethnicity == "X269" ~ "ALANTESU",
        Indigenous_Ethnicity == "X271" ~ "AMAWAKA",
        Indigenous_Ethnicity == "X272" ~ "ANACE",
        Indigenous_Ethnicity == "X273" ~ "APURINA",
        Indigenous_Ethnicity == "X274" ~ "ARANA",
        Indigenous_Ethnicity == "X275" ~ "ARAPACO",
        Indigenous_Ethnicity == "X276" ~ "ARARA APOLIMA",
        Indigenous_Ethnicity == "X277" ~ "ARARA DO ARIPUANA",
        Indigenous_Ethnicity == "X278" ~ "ARIPUANA",
        Indigenous_Ethnicity == "X279" ~ "ASSURINI",
        Indigenous_Ethnicity == "X280" ~ "AWUARA",
        Indigenous_Ethnicity == "X281" ~ "BORBA",
        Indigenous_Ethnicity == "X282" ~ "CABIXI",
        Indigenous_Ethnicity == "X283" ~ "CAMARARE",
        Indigenous_Ethnicity == "X284" ~ "CAMASURI",
        Indigenous_Ethnicity == "X285" ~ "CARA PRETA",
        Indigenous_Ethnicity == "X286" ~ "CHARRUA",
        Indigenous_Ethnicity == "X287" ~ "CUJUBIM",
        Indigenous_Ethnicity == "X288" ~ "DAW",
        Indigenous_Ethnicity == "X289" ~ "GAVIAO",
        Indigenous_Ethnicity == "X290" ~ "GUARANI",
        Indigenous_Ethnicity == "X291" ~ "HALANTESU",
        Indigenous_Ethnicity == "X292" ~ "HALOTESU",
        Indigenous_Ethnicity == "X293" ~ "HENGATU",
        Indigenous_Ethnicity == "X294" ~ "HIXKARYANA",
        Indigenous_Ethnicity == "X295" ~ "HUPDE",
        Indigenous_Ethnicity == "X296" ~ "HUPDES",
        Indigenous_Ethnicity == "X297" ~ "IAUANAUA",
        Indigenous_Ethnicity == "X298" ~ "IAUARETE ACU",
        Indigenous_Ethnicity == "X299" ~ "IKPENG",
        Indigenous_Ethnicity == "X300" ~ "INAMBU",
        Indigenous_Ethnicity == "X301" ~ "INHABARANA",
        Indigenous_Ethnicity == "X302" ~ "JAVAE",
        Indigenous_Ethnicity == "X303" ~ "JENIPAPO",
        Indigenous_Ethnicity == "X304" ~ "JENIPAPO KANINDE",
        Indigenous_Ethnicity == "X305" ~ "JIAHOI",
        Indigenous_Ethnicity == "X306" ~ "KAIOWA",
        Indigenous_Ethnicity == "X307" ~ "KAMPA",
        Indigenous_Ethnicity == "X308" ~ "KANELA",
        Indigenous_Ethnicity == "X309" ~ "KARAFAWYANA",
        Indigenous_Ethnicity == "X310" ~ "KARARAO",
        Indigenous_Ethnicity == "X311" ~ "KARUBO",
        Indigenous_Ethnicity == "X312" ~ "KASSUPA",
        Indigenous_Ethnicity == "X313" ~ "KATITHAULU",
        Indigenous_Ethnicity == "X314" ~ "KATOKIN",
        Indigenous_Ethnicity == "X315" ~ "KATUKINA PANO",
        Indigenous_Ethnicity == "X316" ~ "KATUKINA PEDA DJAPA",
        Indigenous_Ethnicity == "X317" ~ "KATUKINA SHANENAUWA",
        Indigenous_Ethnicity == "X318" ~ "KAXAGO",
        Indigenous_Ethnicity == "X319" ~ "KAYABI",
        Indigenous_Ethnicity == "X320" ~ "KINA WAIMIRI ATROARI",
        Indigenous_Ethnicity == "X321" ~ "KIRIRI BARRA",
        Indigenous_Ethnicity == "X322" ~ "KITHAULU",
        Indigenous_Ethnicity == "X323" ~ "KOIAIA",
        Indigenous_Ethnicity == "X324" ~ "KOIUPANKA",
        Indigenous_Ethnicity == "X325" ~ "KONTANAWA",
        Indigenous_Ethnicity == "X326" ~ "KRAHO KANELA",
        Indigenous_Ethnicity == "X327" ~ "KULINA",
        Indigenous_Ethnicity == "X328" ~ "LATUNDE",
        Indigenous_Ethnicity == "X329" ~ "MAKU",
        Indigenous_Ethnicity == "X330" ~ "MAKUNAMBE",
        Indigenous_Ethnicity == "X331" ~ "MAMAINDE",
        Indigenous_Ethnicity == "X332" ~ "MAMURI",
        Indigenous_Ethnicity == "X333" ~ "MANACAPURU",
        Indigenous_Ethnicity == "X334" ~ "MANAIRISSU",
        Indigenous_Ethnicity == "X335" ~ "MANCHINERI",
        Indigenous_Ethnicity == "X336" ~ "MANDUCA",
        Indigenous_Ethnicity == "X337" ~ "MARIBONDO",
        Indigenous_Ethnicity == "X338" ~ "MASSAKA",
        Indigenous_Ethnicity == "X339" ~ "MAWAYANA",
        Indigenous_Ethnicity == "X340" ~ "MAWE",
        Indigenous_Ethnicity == "X341" ~ "MAYORUNA",
        Indigenous_Ethnicity == "X342" ~ "MIQUELENO",
        Indigenous_Ethnicity == "X343" ~ "MOKURIN",
        Indigenous_Ethnicity == "X344" ~ "MON ORO WARAM",
        Indigenous_Ethnicity == "X345" ~ "MUTUM",
        Indigenous_Ethnicity == "X346" ~ "MYKY",
        Indigenous_Ethnicity == "X347" ~ "NADEB",
        Indigenous_Ethnicity == "X348" ~ "NAMBIKWARA",
        Indigenous_Ethnicity == "X349" ~ "NEGAROTE",
        Indigenous_Ethnicity == "X350" ~ "NHENGATU",
        Indigenous_Ethnicity == "X351" ~ "OFAIE XAVANTE",
        Indigenous_Ethnicity == "X352" ~ "ONCA",
        Indigenous_Ethnicity == "X353" ~ "ORO AT",
        Indigenous_Ethnicity == "X354" ~ "ORO EO",
        Indigenous_Ethnicity == "X355" ~ "ORO JOWIN",
        Indigenous_Ethnicity == "X356" ~ "ORO MIYLIN",
        Indigenous_Ethnicity == "X357" ~ "ORO MON",
        Indigenous_Ethnicity == "X358" ~ "ORO NAO",
        Indigenous_Ethnicity == "X359" ~ "ORO WAM",
        Indigenous_Ethnicity == "X360" ~ "ORO WARAM",
        Indigenous_Ethnicity == "X361" ~ "ORO WARAM XIJEIN",
        Indigenous_Ethnicity == "X362" ~ "PACA",
        Indigenous_Ethnicity == "X363" ~ "PANKARA",
        Indigenous_Ethnicity == "X364" ~ "PAPAGAIO",
        Indigenous_Ethnicity == "X365" ~ "PAYAYA",
        Indigenous_Ethnicity == "X366" ~ "PIPIPAN",
        Indigenous_Ethnicity == "X367" ~ "PIRATA",
        Indigenous_Ethnicity == "X368" ~ "PUROBORA",
        Indigenous_Ethnicity == "X369" ~ "SABANE",
        Indigenous_Ethnicity == "X370" ~ "SANUMA",
        Indigenous_Ethnicity == "X371" ~ "SAWENTESU",
        Indigenous_Ethnicity == "X372" ~ "SILCY TAPUYA",
        Indigenous_Ethnicity == "X373" ~ "SIUCI",
        Indigenous_Ethnicity == "X374" ~ "TABAJARA",
        Indigenous_Ethnicity == "X375" ~ "TAKUARA",
        Indigenous_Ethnicity == "X376" ~ "TATU",
        Indigenous_Ethnicity == "X377" ~ "TAWANDE",
        Indigenous_Ethnicity == "X378" ~ "TEFE",
        Indigenous_Ethnicity == "X379" ~ "TIMBIRA",
        Indigenous_Ethnicity == "X380" ~ "TORA DO BAIXO GRANDE",
        Indigenous_Ethnicity == "X381" ~ "TSUNHUM DJAPA",
        Indigenous_Ethnicity == "X382" ~ "TUBARAO",
        Indigenous_Ethnicity == "X383" ~ "TUPAIU",
        Indigenous_Ethnicity == "X384" ~ "TUPI",
        Indigenous_Ethnicity == "X385" ~ "TUPINAMBA DE BELMONTE",
        Indigenous_Ethnicity == "X386" ~ "URUBU",
        Indigenous_Ethnicity == "X387" ~ "URUBU KAAPOR",
        Indigenous_Ethnicity == "X388" ~ "URUPA",
        Indigenous_Ethnicity == "X389" ~ "WAI WAI",
        Indigenous_Ethnicity == "X390" ~ "WAIKISU",
        Indigenous_Ethnicity == "X391" ~ "WAKALITESU",
        Indigenous_Ethnicity == "X392" ~ "WASSUSU",
        Indigenous_Ethnicity == "X393" ~ "XEREU",
        Indigenous_Ethnicity == "X394" ~ "XI EIN",
        Indigenous_Ethnicity == "X395" ~ "XICRIN",
        Indigenous_Ethnicity == "X396" ~ "XIPAYA",
        Indigenous_Ethnicity == "X397" ~ "XIRIANA",
        Indigenous_Ethnicity == "X398" ~ "XIRUAI",
        Indigenous_Ethnicity == "X399" ~ "YEPAMASSA",
        Indigenous_Ethnicity == "X400" ~ "TIRIYO",
        Indigenous_Ethnicity == "X401" ~ "YANOMAMI",
        Indigenous_Ethnicity == "X402" ~ "ARARA",
        Indigenous_Ethnicity == "X403" ~ "SAKIRIABAR",
        Indigenous_Ethnicity == "X404" ~ "TATZ",
        Indigenous_Ethnicity == "X405" ~ "No Information",
        TRUE ~ ""
      )) %>%
      dplyr::mutate(Indigenous_Ethnicity = as.factor(Indigenous_Ethnicity)) %>%
      dplyr::select(-ETNIA)
  }


  if ("FINANC" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Healthcare_Financing = as.character(FINANC)) %>%
      dplyr::mutate(Healthcare_Financing = dplyr::case_when(
        Healthcare_Financing %in% c("1", "01") ~ "Basic Care (PAB)",
        Healthcare_Financing %in% c("2", "02") ~ "Pharmaceutical Assistance",
        Healthcare_Financing %in% c("4", "04") ~ "Strategic Actions and Compensations Fund (FAEC)",
        Healthcare_Financing %in% c("5", "05") ~ "MAC Incentive",
        Healthcare_Financing %in% c("6", "06") ~ "Medium and High Complexity (MAC)",
        Healthcare_Financing %in% c("7", "07") ~ "Health Surveillance",
        Healthcare_Financing %in% c("0", "00") ~ NA_character_,  # Mapping 0 and 00 to NA
        Healthcare_Financing == "99" ~ NA_character_,             # Mapping 99 to NA
        TRUE ~ ""  # Retain the original value if no match
      )) %>%
      dplyr::mutate(Healthcare_Financing = as.factor(Healthcare_Financing)) %>%
      dplyr::select(-FINANC)  # Remove the original FINANC column
  }






  if ("COMPLEX" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Healthcare_Complexity = as.character(COMPLEX)) %>%
      dplyr::mutate(Healthcare_Complexity = dplyr::case_when(
        Healthcare_Complexity %in% c("1", "01") ~ "Basic Care",
        Healthcare_Complexity %in% c("2", "02") ~ "Medium Complexity",
        Healthcare_Complexity %in% c("3", "03") ~ "High Complexity",
        Healthcare_Complexity %in% c("0", "00") ~ NA_character_,  # Mapping 0 and 00 to NA
        Healthcare_Complexity %in% c("99") ~ NA_character_,       # Mapping 99 to NA
        TRUE ~ Healthcare_Complexity  # Retain the original value if no match
      )) %>%
      dplyr::mutate(Healthcare_Complexity = as.factor(Healthcare_Complexity)) %>%
      dplyr::select(-COMPLEX)  # Remove the original COMPLEX column
  }


  if ("CAR_INT" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Hospitalization_Type = as.character(CAR_INT)) %>%
      dplyr::mutate(Hospitalization_Type = dplyr::case_when(
        Hospitalization_Type %in% c("1", "01") ~ "Elective",
        Hospitalization_Type %in% c("2", "02") ~ "Emergency",
        Hospitalization_Type %in% c("3", "03") ~ "Workplace accident or in company service",
        Hospitalization_Type %in% c("4", "04") ~ "Accident on commute to work",
        Hospitalization_Type %in% c("5", "05") ~ "Other types of traffic accident",
        Hospitalization_Type %in% c("6", "06") ~ "Other types of injury and poisoning by chemical or physical agents",
        TRUE ~ ""  # Retain the original value if no match
      )) %>%
      dplyr::mutate(Hospitalization_Type = as.factor(Hospitalization_Type)) %>%
      dplyr::select(-CAR_INT)  # Remove the original CAR_INT column
  }



  data <- data %>%
    dplyr::mutate(Year_Competency = as.numeric(ANO_CMPT)) %>%
    dplyr::select(-ANO_CMPT)


  data <- data %>%
    dplyr::mutate(Month_Competency = as.numeric(MES_CMPT)) %>%
    dplyr::select(-MES_CMPT)

  if ("CID_MORTE" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(ICD_Death = as.character(CID_MORTE)) %>%
      dplyr::mutate(ICD_Death = as.factor(ICD_Death)) %>%
      dplyr::select(-CID_MORTE)
  }



  if ("MORTE" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Death = as.character(MORTE)) %>%
      dplyr::mutate(Death = dplyr::case_when(
        Death == "0" ~ "No",    # Assuming 0 means the patient did not die
        Death == "1" ~ "Yes",   # Assuming 1 means the patient died
        TRUE ~ NA_character_    # Handling any unknown or missing values
      )) %>%
      dplyr::mutate(Death = as.factor(Death)) %>%
      dplyr::select(-MORTE)
  }


  if ("MARCA_UTI" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(ICU_Usage_Mark = as.character(MARCA_UTI)) %>%
      dplyr::mutate(ICU_Usage_Mark = dplyr::case_when(
        ICU_Usage_Mark %in% c("0", "00")  ~ "Did not use ICU",
        ICU_Usage_Mark == "51" ~ "Adult ICU - Type II COVID-19",
        ICU_Usage_Mark == "52" ~ "Pediatric ICU - Type II COVID-19",
        ICU_Usage_Mark == "74" ~ "Adult ICU - Type I",
        ICU_Usage_Mark == "75" ~ "Adult ICU - Type II",
        ICU_Usage_Mark == "76" ~ "Adult ICU - Type III",
        ICU_Usage_Mark == "77" ~ "Infant ICU - Type I",
        ICU_Usage_Mark == "78" ~ "Infant ICU - Type II",
        ICU_Usage_Mark == "79" ~ "Infant ICU - Type III",
        ICU_Usage_Mark == "80" ~ "Neonatal ICU - Type I",
        ICU_Usage_Mark == "81" ~ "Neonatal ICU - Type II",
        ICU_Usage_Mark == "82" ~ "Neonatal ICU - Type III",
        ICU_Usage_Mark == "83" ~ "Burn ICU",
        ICU_Usage_Mark == "85" ~ "Coronary ICU - UCO Type II",
        ICU_Usage_Mark == "86" ~ "Coronary ICU - UCO Type III",
        ICU_Usage_Mark == "99" ~ "ICU Donor",
        ICU_Usage_Mark %in% c("1", "01")  ~ "Used more than one type of ICU",
        TRUE ~ ""
      )) %>%
      dplyr::mutate(ICU_Usage_Mark = as.factor(ICU_Usage_Mark)) %>%
      dplyr::select(-MARCA_UTI)
  }

  if ("VAL_SH" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Hospital_Service_Value = as.numeric(VAL_SH)) %>%
      dplyr::select(-VAL_SH)
  }


  if ("VAL_TOT" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Total_Cost_BRL = as.numeric(VAL_TOT)) %>%
      dplyr::select(-VAL_TOT)
  }


  if ("US_TOT" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Total_Cost_USD = as.numeric(US_TOT)) %>%
      dplyr::select(-US_TOT)
  }

  if ("GESTAO" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Management_Type = as.character(GESTAO)) %>%
      dplyr::mutate(Management_Type = dplyr::case_when(
        Management_Type %in% c("00") ~ "State",
        Management_Type %in% c("02", "2") ~ "Full State",
        Management_Type %in% c("01", "1") ~ "Full Municipal Assistance",
        Management_Type %in% c("03", "3") ~ NA_character_,
        Management_Type %in% c("09", "9") ~ NA_character_,
        TRUE ~ Management_Type
      )) %>%
      dplyr::mutate(Management_Type = as.factor(Management_Type)) %>%
      dplyr::select(-GESTAO)
  }



  if ("COD_IDADE" %in% names(data)) {
    data <- data %>%
      # First, translate COD_IDADE to Age_Code
      dplyr::mutate(Age_Code = as.character(COD_IDADE)) %>%
      dplyr::mutate(Age_Code = dplyr::case_when(
        Age_Code == "0" ~ NA_character_,        # No age specified
        Age_Code %in% c("02", "2") ~ "Days",    # Age in days
        Age_Code %in% c("03", "3") ~ "Months",  # Age in months
        Age_Code %in% c("04", "4") ~ "Years",   # Age in years
        Age_Code %in% c("05", "5") ~ "Centenarian (100+)", # Age for people 100+ years old
        TRUE ~ ""  # Use an empty string for all other cases
      )) %>%
      dplyr::mutate(Age_Code = as.factor(Age_Code)) %>%

      # Ensure IDADE is numeric
      dplyr::mutate(IDADE = as.numeric(IDADE)) %>%

      # Now, create AGE_YEARS based on Age_Code
      dplyr::mutate(AGE_YEARS = dplyr::case_when(
        Age_Code == "Days"   ~ 0,          # Keep as 0 for days
        Age_Code == "Months" ~ 0,          # Keep as 0 for months
        Age_Code == "Years"  ~ IDADE,      # Already in years, no changes needed
        Age_Code == "Centenarian (100+)" ~ IDADE + 100,  # Add 100 to the age
        TRUE ~ NA_real_  # Handle missing or unknown cases
      )) %>%

      # Rename IDADE to AGE_UNSPECIFIED (keeping original data)
      dplyr::rename(AGE_UNSPECIFIED = IDADE) %>%

      # Remove COD_IDADE after translation
      dplyr::select(-COD_IDADE)
  }




  if ("NATUREZA" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Institution_Type = as.character(NATUREZA)) %>%
      dplyr::mutate(Institution_Type = dplyr::case_when(
        Institution_Type == "0"  ~ NA_character_,
        Institution_Type == "99" ~ NA_character_,
        Institution_Type == "10" ~ "Own",
        Institution_Type == "20" ~ "Contracted",
        Institution_Type == "22" ~ "Contracted opting for SIMPLES",
        Institution_Type == "30" ~ "Federal",
        Institution_Type == "31" ~ "Federal with Own Funds",
        Institution_Type == "40" ~ "State",
        Institution_Type == "41" ~ "State with Own Funds",
        Institution_Type == "50" ~ "Municipal",
        Institution_Type == "60" ~ "Philanthropic",
        Institution_Type == "61" ~ "Philanthropic exempt from taxes and social contributions",
        Institution_Type == "63" ~ "Philanthropic exempt from income tax and contributions on net income",
        Institution_Type == "70" ~ "University for Teaching",
        Institution_Type == "80" ~ "Union",
        Institution_Type == "90" ~ "University for Research",
        Institution_Type == "91" ~ "University for Research exempt from taxes and social contributions",
        Institution_Type == "93" ~ "University for Research exempt from income tax and contributions on net income",
        Institution_Type == "94" ~ "Private university for teaching and research",
        Institution_Type == "92" ~ "Private university for teaching and research",
        TRUE ~ Institution_Type
      )) %>%
      dplyr::mutate(Institution_Type = as.factor(Institution_Type)) %>%
      dplyr::select(-NATUREZA)
  }



  if ("ESPEC" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Specialty = as.character(ESPEC)) %>%
      dplyr::mutate(Specialty = dplyr::case_when(
        Specialty == "1"  ~ "Surgical",
        Specialty == "2"  ~ "Obstetric",
        Specialty == "3"  ~ "Clinical",
        Specialty == "4"  ~ "Chronic",
        Specialty == "5"  ~ "Psychiatry",
        Specialty == "6"  ~ "Sanitary Pneumology (tuberculosis)",
        Specialty == "7"  ~ "Pediatric",
        Specialty == "8"  ~ "Rehabilitation",
        Specialty == "9"  ~ "Day Bed / Surgical",
        Specialty == "10" ~ "Day Bed / AIDS",
        Specialty == "11" ~ "Day Bed / Cystic Fibrosis",
        Specialty == "12" ~ "Day Bed / Post-Transplant Complication",
        Specialty == "13" ~ "Day Bed / Geriatrics",
        Specialty == "14" ~ "Day Bed / Mental Health",
        Specialty == "51" ~ "Adult ICU II - COVID-19",
        Specialty == "52" ~ "Pediatric ICU II - COVID-19",
        Specialty == "64" ~ "Intermediate Unit",
        Specialty == "65" ~ "Neonatal Intermediate Unit",
        Specialty == "74" ~ "ICU I",
        Specialty == "75" ~ "Adult ICU II",
        Specialty == "76" ~ "Adult ICU III",
        Specialty == "77" ~ "Infant ICU I",
        Specialty == "78" ~ "Infant ICU II",
        Specialty == "79" ~ "Infant ICU III",
        Specialty == "80" ~ "Neonatal ICU I",
        Specialty == "81" ~ "Neonatal ICU II",
        Specialty == "82" ~ "Neonatal ICU III",
        Specialty == "83" ~ "Burn ICU",
        Specialty == "84" ~ "Night Admission",
        Specialty == "85" ~ "Coronary ICU-UCO type II",
        Specialty == "86" ~ "Coronary ICU-UCO type III",
        Specialty == "87" ~ "Mental Health (Clinical)",
        Specialty == "88" ~ "Burn Adult (Clinical)",
        Specialty == "89" ~ "Burn Pediatric (Clinical)",
        Specialty == "90" ~ "Burn Adult (Surgical)",
        Specialty == "91" ~ "Burn Pediatric (Surgical)",
        Specialty == "92" ~ "Neonatal Intermediate Care Unit Conventional",
        Specialty == "93" ~ "Neonatal Intermediate Care Unit Kangaroo",
        Specialty == "94" ~ "Pediatric Intermediate Care Unit",
        Specialty == "95" ~ "Adult Intermediate Care Unit",
        Specialty == "96" ~ "Pulmonary Ventilatory Support - COVID-19",
        TRUE ~ Specialty
      )) %>%
      dplyr::mutate(Specialty = as.factor(Specialty)) %>%
      dplyr::select(-ESPEC)
  }



  # Process DIAG_PRINC variable
  if ("DIAG_PRINC" %in% names(data)) {
    # Ensure DIAG_PRINC is character
    data$DIAG_PRINC <- as.character(data$DIAG_PRINC)

    # Left join with the icd_codes dataset to get the descriptions
    data <- data %>%
      dplyr::left_join(icd_codes, by = c("DIAG_PRINC" = "code"))

    data <- data %>%
      dplyr::rename(ICD_10_MD = DIAG_PRINC)
    # Optionally rename the Description column
    data <- data %>%
      dplyr::rename(Main_Diagnosis = Description)
  }

  # Process DIAG_PRINC variable
  if ("PROC_REA" %in% names(data)) {
    # Ensure DIAG_PRINC is character
    data$PROC_REA <- as.character(data$PROC_REA)

    # Left join with the proce_rea dataset to get the descriptions
    data <- data %>%
      dplyr::left_join(proc_rea, by = c("PROC_REA" = "code"))

    data <- data %>%
      dplyr::rename(Procedure_Code = PROC_REA)
    # Optionally rename the Description column
    data <- data %>%
      dplyr::rename(Main_Procedure = PROCEDURE)
  }






  # Process SEXO variable
  if ("SEXO" %in% names(data)) {
    data$SEXO <- dplyr::case_when(
      data$SEXO == "1" ~ "Male",
      data$SEXO == "2" ~ "Female",
      data$SEXO == "3" ~ "Female",  # Treat code "3" as Female
      data$SEXO %in% c("0", "9") ~ NA_character_,
      TRUE ~ NA_character_
    )
    data$sex <- as.factor(data$SEXO)
    data <- data %>% dplyr::select(-SEXO)
  }




  if ("MUNIC_MOV" %in% names(data)) {
    # Renomeia MUNIC_MOV para Hospital_CityCod em 'data'
    data <- data %>%
      dplyr::rename(Hospital_CityCod = MUNIC_MOV)

    # Converte ambas as colunas para numérico
    data$Hospital_CityCod <- as.numeric(data$Hospital_CityCod)
    ibge$Hospital_CityCod <- as.numeric(ibge$Hospital_CityCod)

    # Garante que se o código começar com "53", ele seja alterado para "530010"
    data <- data %>%
      dplyr::mutate(Hospital_CityCod = ifelse(substr(Hospital_CityCod, 1, 2) == "53", 530010, Hospital_CityCod))

    # Realiza o left join
    data <- data %>%
      dplyr::left_join(ibge, by = "Hospital_CityCod")
  }

  if ("MUNIC_RES" %in% names(data)) {
    # Renomeia MUNIC_RES para Patient_CityCod em 'data'
    data <- data %>%
      dplyr::rename(Patient_CityCod = MUNIC_RES)

    # Converte a coluna Patient_CityCod para numérico
    data$Patient_CityCod <- as.numeric(data$Patient_CityCod)

    # Renomeia as variáveis de Hospital* para Patient* diretamente em ibge
    ibge <- ibge %>%
      dplyr::rename_with(~ gsub("^Hospital", "Patient", .), starts_with("Hospital"))

    # Converte a coluna Patient_CityCod de ibge para numérico
    ibge$Patient_CityCod <- as.numeric(ibge$Patient_CityCod)

    # Garante que se o código começar com "53", ele seja alterado para "530010"
    data <- data %>%
      dplyr::mutate(Patient_CityCod = ifelse(substr(Patient_CityCod, 1, 2) == "53", 530010, Patient_CityCod))

    # Realiza o left join com o dataset ibge modificado
    data <- data %>%
      dplyr::left_join(ibge, by = "Patient_CityCod")
  }



  # Process RACA_COR variable
  if ("N_AIH" %in% names(data)) {
    # Ensure RACA_COR is treated as character for consistency
    data$ID <- as.character(data$N_AIH)
    data <- data %>% dplyr::select(-N_AIH)
  }
  # Process RACA_COR variable
  if ("DIAG_SECUN" %in% names(data)) {
    # Ensure RACA_COR is treated as character for consistency
    data$ICD_10_SD <- as.character(data$DIAG_SECUN)
    data <- data %>% dplyr::select(-DIAG_SECUN)
  }


  # IDENT
  if("IDENT" %in% names(data)){
    data <- data %>%
      dplyr::mutate(IDENT = as.character(.data$IDENT)) %>%
      dplyr::mutate(IDENT = dplyr::case_when(
        IDENT %in% c("1", "01") ~ "Primary",
        IDENT %in% c("3", "03") ~ "Continuation",
        IDENT %in% c("5", "05") ~ "Long stay",
        TRUE ~ IDENT
      )) %>%
      dplyr::mutate(IDENT = as.factor(IDENT))
  }




  # Process RACA_COR variable
  if ("RACA_COR" %in% names(data)) {
    # Ensure RACA_COR is treated as character for consistency
    data$RACA_COR <- as.character(data$RACA_COR)

    data$RACA_COR <- dplyr::case_when(
      data$RACA_COR %in% c("1", "01") ~ "White",
      data$RACA_COR %in% c("2", "02") ~ "Black",
      data$RACA_COR %in% c("3", "03") ~ "Brown",
      data$RACA_COR %in% c("4", "04") ~ "Yellow",
      data$RACA_COR %in% c("5", "05") ~ "Indigenous",
      data$RACA_COR %in% c("0", "9", "00", "09") ~ NA_character_,
      TRUE ~ NA_character_
    )

    data$race <- as.factor(data$RACA_COR)
    data <- data %>% dplyr::select(-RACA_COR)
  }


  # Process NASC variable (birth date)
  if ("NASC" %in% names(data)) {
    data$Birthday <- as.Date(data$NASC, format = "%Y%m%d")
    data <- data %>% dplyr::select(-NASC)
  }

  # Process admission (DT_INTER) and discharge (DT_SAIDA) dates
  if ("DT_INTER" %in% names(data)) {
    data$DT_HOSP <- as.Date(data$DT_INTER, format = "%Y%m%d")
    data <- data %>% dplyr::select(-DT_INTER) # Remove DT_SAIDA
  }

  if ("DT_SAIDA" %in% names(data)) {
    data$DT_DISCHARGE <- as.Date(data$DT_SAIDA, format = "%Y%m%d")
    data <- data %>% dplyr::select(-DT_SAIDA) # Remove DT_SAIDA
  }




  if("DIAS_PERM" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(`LOS(days)` = as.numeric(.data$DIAS_PERM)) %>%
      dplyr::select(-DIAS_PERM)  # Remove DIAS_PERM after conversion
  }


  if("US_TOT" %in% names(data)){
    data <- data %>%
      dplyr::mutate(Total_Cost_USD = as.numeric(.data$US_TOT))
    data <- data %>% dplyr::select(-US_TOT)
  }

  # COBRANCA (Reason for discharge/stay, according to SAS ordinance 719)
  if ("COBRANCA" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(COBRANCA = as.character(.data$COBRANCA)) %>%
      dplyr::mutate(outcome = dplyr::case_when(
        .data$COBRANCA == "11" ~ "Discharged: Cured",
        .data$COBRANCA == "12" ~ "Discharged: Improved",
        .data$COBRANCA == "14" ~ "Discharged: At request",
        .data$COBRANCA == "15" ~ "Discharged: Return for follow-up",
        .data$COBRANCA == "16" ~ "Discharged: Due to evasion",
        .data$COBRANCA == "18" ~ "Discharged: Other reasons",
        .data$COBRANCA == "19" ~ "Discharged: Acute patient in psychiatry",
        .data$COBRANCA == "21" ~ "Extended stay: Disease-specific characteristics",
        .data$COBRANCA == "22" ~ "Extended stay: Complications",
        .data$COBRANCA == "23" ~ "Extended stay: Socio-familial reasons",
        .data$COBRANCA == "24" ~ "Extended stay: Living donor (organ, tissue, cell donation)",
        .data$COBRANCA == "25" ~ "Extended stay: Deceased donor (organ, tissue, cell donation)",
        .data$COBRANCA == "26" ~ "Extended stay: Procedure change",
        .data$COBRANCA == "27" ~ "Extended stay: Reoperation",
        .data$COBRANCA == "28" ~ "Extended stay: Other reasons",
        .data$COBRANCA == "29" ~ "Transferred: Home hospitalization",
        .data$COBRANCA == "32" ~ "Transferred: Home hospitalization",
        .data$COBRANCA == "31" ~ "Transferred: Another facility",
        .data$COBRANCA == "41" ~ "Death: Certificate issued by attending physician",
        .data$COBRANCA == "42" ~ "Death: Certificate issued by Medical Examiner",
        .data$COBRANCA == "43" ~ "Death: Certificate issued by SVO",
        .data$COBRANCA == "51" ~ "Administrative closure",
        .data$COBRANCA == "61" ~ "Discharged: Mother/puerpera and newborn",
        .data$COBRANCA == "17" ~ "Discharged: Mother/puerpera and newborn",
        .data$COBRANCA == "62" ~ "Discharged: Mother/puerpera, newborn stays",
        .data$COBRANCA == "13" ~ "Discharged: Mother/puerpera, newborn stays",
        .data$COBRANCA == "63" ~ "Discharged: Mother/puerpera, newborn death",
        .data$COBRANCA == "64" ~ "Discharged: Mother/puerpera with fetal death",
        .data$COBRANCA == "65" ~ "Death: Pregnant woman and conceptus",
        .data$COBRANCA == "66" ~ "Death: Mother/puerpera, newborn discharged",
        .data$COBRANCA == "67" ~ "Death: Mother/puerpera, newborn stays",
        TRUE ~ .data$COBRANCA  # Default case for unknown values
      )) %>%
      dplyr::mutate(outcome = as.factor(.data$outcome)) %>%
      dplyr::select(-COBRANCA)  # Remove COBRANCA column after processing
  }


  if ("VAL_SP" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Professional_Service_Value = as.numeric(VAL_SP)) %>%
      dplyr::select(-VAL_SP)
  }



  # Unescape unicode characters for all character columns
  char_cols <- sapply(data, is.character)
  data[char_cols] <- lapply(data[char_cols], stringi::stri_unescape_unicode)

  # Remove unused factor levels
  data <- droplevels(data)



  col_order <- c(
    "ID",                  # First variable (ID)
    "DT_HOSP",             # Admission date
    "DT_DISCHARGE",
    "LOS(days)",
    "Main_Procedure",      # Procedure name
    "Main_Diagnosis",      # Main diagnosis
    "ICD_10_SD",           # Secondary diagnosis
    "sex",                 # Gender
    "AGE_YEARS",           # Age in years
    "AGE_UNSPECIFIED",     # Unspecified age
    "Age_Code",            # Age code (Days, Months, Years, etc.)
    "race",                # Race/ethnicity
    "Total_Cost_USD",      # Total cost in USD
    "Death",
    "Birthday",
    "outcome"
  )

  # Select columns in the desired order, keeping only those that exist in the dataset
  col_order <- intersect(col_order, names(data))
  data <- dplyr::select(data, all_of(col_order), everything())


  return(data)
}

