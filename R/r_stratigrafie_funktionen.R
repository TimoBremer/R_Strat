# --- --- --- --- --- FUNKTION ergaenzen_listen --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

# Uebersetzt 1. die Unter-Liste in eine ueber-Liste und fuehrt diese mit der bestehenden ueberliste-zusammen
# 2. wird die Gleich-Liste um umgekehrte Ausdruecke ergaenzt (3 = 5 und 5 = 3)

ergaenzen_listen <- function(liste_zu_ergaenzen, liste_zu_veraendern = liste_zu_ergaenzen)

{
  # Drehen der Unter-Werte. geht nicht einfach mit "liste_zu_veraendern[,c(3,2,1)]", da der folgende rbind-Befehl
  # ...das Ganze automatisch zurueckdreht
  liste_zu_veraendern_2 <- liste_zu_veraendern
  liste_zu_veraendern_2[,1] <- liste_zu_veraendern[,3]
  liste_zu_veraendern_2[,3] <- liste_zu_veraendern[,1]

  # zusammenfuehren:
  liste_zu_ergaenzen <- rbind(liste_zu_ergaenzen, liste_zu_veraendern_2)
  # Loeschen von Zeilen, die nicht zugewiesen (NA) sind
  liste_zu_ergaenzen <- na.omit(liste_zu_ergaenzen)
  # Redundanzen loeschen:
  liste_zu_ergaenzen <- unique(liste_zu_ergaenzen)

  # da die Liste unter gedreht wurde, kann nun "unter" durch "ueber" ersetzt werden:
  # ...das bugt, wenn die Tabelle keine Zeilen hat (passiert i.d.R. bei "gleich").
  #...daher ist Schleife zu Pruefung erforderlich
  if (nrow(liste_zu_ergaenzen) > 0)
  {
    liste_zu_ergaenzen[liste_zu_ergaenzen == "unter"] = "ueber"
  }

  analysis_functions_objects()

  return(liste_zu_ergaenzen)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- FUNKTION befuellen_matrix --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

## Funktion zum Befuellen Matrix, Variablen sind die Grundmatrix und eine der Listen "ueber..." oder "unter..."
befuellen_matrix <- function(ausgangsmatrix, liste_ueber_etc)

{
  ## muss vorab geprueft werden, ob list_ueber_etc ueberhaupt Daten enth?lt
  if (nrow(liste_ueber_etc) > 0)
  {
    ## Beginn for-Loop
    for(fortl_nr in 1:nrow(liste_ueber_etc))
    {
      ## befuellt die Matrix mit den Werten aus liste_ueber etc., die Spaltenweise abgearbeitet werden
      ausgangsmatrix[(array(liste_ueber_etc[fortl_nr,1])),(array(liste_ueber_etc[fortl_nr,2]))] <- 1
      #print(paste("fortlaufende Nr.: " , fortl_nr , "; ", liste_ueber_etc[,1], "ueber ", liste_ueber_etc[,3] , "Ende"))
    } ## Ende for-Loop
  }

  analysis_functions_objects()

  return(ausgangsmatrix)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- FUNKTION gleichsetzen_spalten --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

## setzt zwei Spalten in der Tabelle Matrix-Ueber-unter gleich; Vorraussetzung ist, dass keine Logikfehler entstehen, was im unteren Teil der Funktion geprueft wird
gleichsetzen_spalten <- function(tabelle, zu_veraendernde_zeile, vorlage_zeile)

{
  ## Erstellen einer Matrix, die nur die zwei Zeilen hat, die gleichgesetzt werden sollen:
  temp_matrix <- tabelle[c(zu_veraendernde_zeile,vorlage_zeile),]
  ## vorlaeufiges Gleichsetzen der beiden Spalten:
  tabelle[zu_veraendernde_zeile,] <- apply(temp_matrix,2,max)

  analysis_functions_objects()

  return(tabelle)
} ## Ende Funktion



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- FUNKTION widerspruchsanalyse --- --- --- --- --- --- ----

widerspruchsanalyse <- function(zeilennr, spaltennr, initiale_pruefung, fehler_loeschen = FALSE, matrix_gleich, matrix_ueber_unter, liste_ueber_temp, liste_gleich_temp)
  # innerer Kern der Widerspruchs-Analyse, mit Auswahl, ob Fehler geloescht werden sollen oder Programm abbricht
  # wenn hier ein Widerspruch auftreten wuerde (die Funktion gebraucht wuerde) muesste sie mit der nicht angewandten Funktion "Fehlerkorrektur"...
  #...verknuepft werden, die Schleife muesste unterbrochen, der Fehler gefunden und neu begonnen werden:
  # der Schalter "initiale_pruefung" ist erforderlich, da beim ersten Durchlauf keine Suche der Fehlerkette erforderlich ist
  # ...der Fehler ergibt sich direkt aus den eingegebenen Werten
  # throws out nothing if no conflicts were found, updated tables if "fehler_loeschen = TRUE" or otherwise a conflict report

{
  abbrechen <- FALSE
  correction <- FALSE
  conflict <- "no conflicts found"
  widerspruchskette <- "none"
  befundname_zeile <- row.names(matrix_gleich)[zeilennr]
  befundname_spalte <- row.names(matrix_gleich)[spaltennr]
  #print(matrix_gleich)
  ## 1. "Kehrwerte" (z. B. 3 ueber 4 und 4 ueber 3):
  if (matrix_ueber_unter[zeilennr, spaltennr] == 1 && matrix_ueber_unter[spaltennr, zeilennr] == 1 && !(spaltennr == zeilennr))
  {
    if(fehler_loeschen == TRUE)
    {
      ## Widerspruch wird beidseitig geloescht (z. B. 3 ueber 4 und 4 ueber 3):
      print(paste("Widerspruch Kehrwert aufgetreten in Zeile:" , zeilennr, "corrigated"))
      matrix_ueber_unter[zeilennr, spaltennr] <- 0
      matrix_ueber_unter[spaltennr, zeilennr] <- 0
      correction <- TRUE
    }
    else if (initiale_pruefung == TRUE)
      # beim ersten Durchlauf muss nicht die Funktion "suche_fehlerkette" aktiviert werden,
      # ...da der Fehler aus den Eingangsdaten ermittelbar ist
    {
      widerspruch_teil_i <- c(befundname_zeile, "ueber", befundname_spalte)
      widerspruch_teil_ii <- c(befundname_zeile, "unter", befundname_spalte)
      abbrechen <- TRUE
      widerspruchskette <- ausgabe_widerspruchskette(widerspruch_teil_i, widerspruch_teil_ii)
      conflict <- paste("abgebrochen, da Kehrwert aufgetreten in Zeile: ", zeilennr)
    }
    else
    {
      widerspruchskette <- suche_fehlerkette(befundname_zeile, liste_ueber_temp, liste_gleich_temp)
      abbrechen <- TRUE
      conflict <- paste("abgebrochen, da Kehrwert aufgetreten in Zeile: ", zeilennr)
    }
  }

  ## Wert gleichzeitig ueber und gleich (3 ueber 3 und 3 = 3):
  if (matrix_ueber_unter[zeilennr, spaltennr] == 1 && matrix_gleich[zeilennr, spaltennr] == 1)
  {
    if(fehler_loeschen == TRUE)
    {
      print(paste("Widerspruch ueber/unter aber auch gleich aufgetreten in Zeile:" , zeilennr, "corrigated"))
      matrix_ueber_unter[zeilennr, spaltennr] <- 0
      matrix_gleich[zeilennr, spaltennr] <- 0
      correction <- TRUE
    }
    else if (initiale_pruefung == TRUE)
    {
      # beim ersten Durchlauf muss nicht die Funktion "suche_fehlerkette" aktiviert werden,
      # ...da der Fehler aus den Eingangsdaten ermitellbar ist

      widerspruch_teil_i <- c(befundname_zeile, "ueber", befundname_spalte)
      widerspruch_teil_ii <- c(befundname_zeile, "gleich", befundname_spalte)
      widerspruchskette <- ausgabe_widerspruchskette(widerspruch_teil_i, widerspruch_teil_ii)
      abbrechen <- TRUE
      conflict <- paste("Widerspruch ueber/unter aber auch gleich aufgetreten in Zeile:" , zeilennr, "Spalte:", spaltennr)
    }
    else
    {
      #hier ist der Fehler!!!
      widerspruchskette <- suche_fehlerkette(befundname_zeile, liste_ueber_temp, liste_gleich_temp)
      abbrechen <- TRUE
      conflict <- paste("Widerspruch ueber/unter aber auch gleich aufgetreten in Zeile:" , zeilennr, "Spalte:", spaltennr)
    }
  }

  ## Diagonale besetzt in der ueber-Unter-Tabelle (3 ueber 3):
  ## dieser Fall wird gleich geloescht, da es sich um einen offensichtlichen Fehler handelt...
  ##...ohne sinnvolle Korrekturoptionen
  if(spaltennr == 1)
    ## dieser Test soll nur einmal bei der ersten Spalte der Zeile durchgefuehrt werden...
    ##...da sonst bei jeder Spalte der Zeile der gleiche Test durchlaeuft
  {
    if (matrix_ueber_unter[zeilennr, zeilennr] == 1)
    {
      matrix_ueber_unter[zeilennr, zeilennr] <- 0
      conflict <- paste("Diagonale freigeraeumt in ueber-Unter-Tabelle, Zeile:" , zeilennr)
    }
  }

## the output of the function is depending whether it was called with the option "fehler_loeschen = TRUE" or "FALSE".
## ...In the first case it returns a corrected version of the stratigraphic tables
## ...in the latter case a report of conflict
  if (fehler_loeschen == TRUE && correction == TRUE)  ## the function returns a corrigated list if "fehler loeschen" is taged
  {
    tab_under_above_and_equal <- list(corr_req = correction, break_req = abbrechen, tab_under_above = matrix_ueber_unter, tab_equal = matrix_gleich)
    return(tab_under_above_and_equal)
  }
  else ## otherwise it returns a report of conflict
  {
    report_of_conflict <- list(corr_req = correction, break_req = abbrechen, Conflict = conflict, chain_of_conflict = widerspruchskette)

    analysis_functions_objects()

    return(report_of_conflict)
  }
}



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- FUNKTION ausgabe_widerspruchskette --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

ausgabe_widerspruchskette <- function(bisherige_widerspruchskette, ergaenzung)
{
  trennlinie_schichten <- c("........", "........", "........")
  widerspruchskette <- rbind(ergaenzung, trennlinie_schichten, bisherige_widerspruchskette)

  analysis_functions_objects()

  return(widerspruchskette)
}



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- FUNKTION Suche Fehlerkette --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

suche_fehlerkette <- function(gesuchte_stelle, liste_ueber_temp, liste_gleich_temp)
{
  print(paste("gesuchte Stelle:" , gesuchte_stelle));
  ## Wenn in der Funktion "Widerspruchsanalyse" ein Fehler aufgetreten ist, wird diese Funktion aufgerufen
  ## verfolgt die Fehlerkette in den urspruenglichen Daten ("liste_alle_beziehungen") ausgehend von der Stelle, in der der Fehler festgestellt wurde

  liste_alle_beziehungen <- rbind(liste_ueber_temp, liste_gleich_temp)

  # fuer den ersten Durchlauf ist die Eingangsliste gleich der Restliste:
  liste_rest <- liste_alle_beziehungen
  listennummer <- 1
  # fuer den initialen Durchlauf ist die Abfrage gleich der gesuchten Stelle
  abfrage <- gesuchte_stelle

  # --- --- ---  Schleife Abfrage Hinweg --- --- ---
  # in dieser Schleife werden alle Beziehungen zur gesuchten Stelle in einzelne Listen geschrieben
  # ...bis die gesuchte Stelle wieder auftaucht, d.h. eine Schleife und somit ein Widerspruch gefunden wurde (1....1).
  # ...Die Listen sind dabei hierarchisch: die Beziehungen in "liste_3" sind daher drei Beziehungen von der gesuchten Stelle entfernt
  # ...es ergibt sich somit eine hierarchische Datenbankstruktur
  repeat
  {
    # zuvor getrennte Listen werden wieder verbunden (s.u.):
    # ..."liste_rest_gleich" gibt es erst nach erstem Durchgang
    # ...Verbindung ergibt erst in drittem Durchgang Sinn
    if(listennummer > 2)
    {
      liste_rest <- rbind(liste_rest, liste_rest_gleich_2)
    }
    # alle Datensaetze abfragen, die gesuchtes Attribut enthalten:
    abgefragte_liste <- subset(liste_rest, liste_rest[,1] %in% abfrage)

    # Liste mit restlichen Beziehungen erstellen, denn einmal verwendete Beziehungen werden (wahrscheinlich) nicht mehr gebraucht:
    liste_rest <- subset(liste_rest, !(liste_rest[,1] %in% abfrage))

    ## Gegenwerte bei gleich loeschen:
    # zuerst eine Abfrage mit allen Gleich-Werten, die bei diesem Durchlauf verwendet wurden:
    abgefragte_liste_gleich <- subset(abgefragte_liste, abgefragte_liste[,2] == "gleich")
    # rechte Seite der abgefragten Gleich-Beziehungen:
    abgefragte_liste_gleich <- array(abgefragte_liste_gleich[,3])

    # beim zweiten Durchgang muessen die gesicherten Gleich-Beziehungen verschoben werden:
    if(listennummer > 1)
    {
      liste_rest_gleich_2 <- liste_rest_gleich
    }

    # Sichern der Gegenwerte fuer die uebernaechste Runde:
    liste_rest_gleich <- subset(liste_rest, liste_rest[,1] %in% abgefragte_liste_gleich & liste_rest[,2] == "gleich" & liste_rest[,3] %in% abfrage)
    # Entfernen der Gegenwerte fuer eine Runde, bei 1 = 3 z.B. 3:
    liste_rest <- subset(liste_rest, !(liste_rest[,1] %in% abgefragte_liste_gleich & liste_rest[,2] == "gleich" & liste_rest[,3] %in% abfrage))

    # nun die neue Abfrage erstellen:
    abfrage <- array(abgefragte_liste[,3])



    # --- --- --- fortlaufende Liste mit Fehlerkette erstellen --- --- ---
    # mit "assign" werden aus der letzten abgefragten Liste zusammen mit der Listennummer fortlaufende Listen erstellt (liste_1, liste_2 usw.):
    assign(paste("liste_", listennummer, sep=''), abgefragte_liste)
    listennummer <- listennummer + 1

    # pruefen, ob Weg zu gesuchter Stelle gefunde wurde:
    # hier fehlt noch irgendeine Art error.handler, damit die Schleife bei einem Fehler nicht unendlich weiterlaeuft!!!
    if (gesuchte_stelle %in% abfrage) break()
    # Sicherung gegen Endlossschleife: wenn mehr Durchlaeufe als Beziehungen:
    if (listennummer > nrow(liste_alle_beziehungen))
    {
      stop("das Programm konnte keinen Widerspruch in der Stratigrafie ermitteln, was auf einen internen Fehler zurueckzufuehren ist!")
    }
  }


# --- --- --- Vorbereitung Rueckweg von Fehler zu Ausgangspunkt --- --- ---
  # da die Listen bislang alle Beziehungen enthalten die von der Fehlerursache ausgehen, muss diese nun rueckwaerts durchlaufen werden
  # ...damit am Ende ein einziger Pfad als Fehlerkette uebrig bleibt
  # ...entspricht im Grunde einer Datenbankabfrage der "gesuchten_stelle" ueber alle Tabelle (z. B. liste_5, liste_4...)
  # ...aehnelt sehr stark der obigen Schleife

  # zunaechst muss die listennummer um 2 herabgesetzt werden, da im letzten Schritt in der obigen Schleife keine Liste mehr angelegt wurde:
  listennummer <- listennummer - 1

  # Abfrage wird wieder gleich gesuchte Stelle gesetzt:
  abfrage <- gesuchte_stelle

  # Tabelle Widerspruchskette muss vorab definiert werden:
  # muss nacher noch schoener geloest werden, ist schließlich die Fehlerausgabe fuer den Benutzer!!!
  widerspruchskette <- matrix(data=0, nrow = 0, ncol = 3)

# --- --- --- Schleife Rueckweg --- --- ---
  # jetzt wird die Listennummer rueckwaerts gezaehlt
  for (listennummer in listennummer:1)
  {
    # aus der Listennummer wird die aktuelle Liste erstellt:
    abgefragte_liste <- eval(as.name(paste("liste_", listennummer, sep='')))
    abgefragte_liste <- subset(abgefragte_liste, abgefragte_liste[,3] %in% abfrage)
    # und wieder mit "assign" wird die "abgefragte_liste" erneut an die entsprechende Liste "liste_1", "liste_2" etc. uebergeben:
    assign(paste("liste_", listennummer, sep=''), abgefragte_liste)
    # hiermit wird der Wert fuer die naechste Abfrage im folgenden Durchlauf geholt:
    abfrage <- array(abgefragte_liste[,1])

    widerspruchskette <- ausgabe_widerspruchskette(widerspruchskette, abgefragte_liste)
  }

  analysis_functions_objects()


  return(widerspruchskette)

    # Einzellisten koennte man in grosse Liste zusammenfuehren, weiss aber noch nicht, wozu das nuetzlich ist
    # kann vielleicht mehr Listen aufnehmen oder ist schneller?
}



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- FUNKTION liste_alle_befunde_mit_dat --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

liste_alle_befunde_mit_dat <- function(liste_stellen, absolute_data_list, matrix1)
{

  liste_alle_befunde_mit_dat <- matrix(data=NA, nrow = length(liste_stellen), ncol = 2)
  colnames(liste_alle_befunde_mit_dat)  <- c(colnames(absolute_data_list))

  liste_alle_befunde_mit_dat[,1] <- liste_stellen

  liste_alle_befunde_mit_dat <- as.matrix(liste_alle_befunde_mit_dat)
  absolute_data_list <- as.matrix(absolute_data_list)

  liste_alle_befunde_mit_dat <- rbind(liste_alle_befunde_mit_dat, absolute_data_list)#hier wird das irgendwie zu einer Liste :(, durch as.matrix verhindert

  # Sortieren der Liste
  liste_alle_befunde_mit_dat <- liste_alle_befunde_mit_dat[order(liste_alle_befunde_mit_dat[,1]),]

  #Loeschen doppelter Stellen
  doppelte_stellen <- which(duplicated(liste_alle_befunde_mit_dat[,1]))-1
  liste_alle_befunde_mit_dat <- liste_alle_befunde_mit_dat[-doppelte_stellen,]


  # Uebertragen der Gleich Beziehungen aus der Matrix-Gleich in die liste_alle_befunde_mit_dat
  for(zeilen_nr in 1:nrow(liste_alle_befunde_mit_dat))
  {
    if (is.na(liste_alle_befunde_mit_dat[zeilen_nr,2]) == FALSE )# nimmt die absolut datierten Stellen aus der liste_alle_befunde_mit_dat,...
    {
      for(spalten_nr in 1:ncol(matrix1))# ...geht in entsprechenden Zeilen der matrix1,...
      {
        if(matrix1[zeilen_nr,spalten_nr] != 0 && (is.na(liste_alle_befunde_mit_dat[spalten_nr,2]) || liste_alle_befunde_mit_dat[spalten_nr,2] < liste_alle_befunde_mit_dat[zeilen_nr,2])  )# ...ueberpr?ft dort welche Stellen gleich sind, ob das Pendant noch kein absolutes Datum hat und ?berpr?ft, ob ein j?ngeres Datum erzielt werden kann...
        {
          liste_alle_befunde_mit_dat[spalten_nr,2] <- liste_alle_befunde_mit_dat[zeilen_nr,2]# ...und ?bertr?gt die Datierungen in die liste_alle_befunde_mit_dat.
        }# Ende if-loop2
      }# Ende for-loop2
    }# Ende if-loop1
  }# Ende for-loop1


  analysis_functions_objects()

  return(liste_alle_befunde_mit_dat)

}#end function



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- FUNKTION kuerzen_matrix --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

kuerzen_matrix <- function(matrix1)
{
  #Loeschen absolut datierter Zeilen... (Alle ausser NAs)
  no_NA_index <- which(!is.na(matrix1[2:nrow(matrix1),1]))+1   #gibt Vektor mit absolut datierten Zeilen an.
  #2:nrow, da die erste Zeile die Datierungen enthaelt.
  #"+1", da wegen 2:ncol die zweite Spalte als erste gez?hlte wird etc.
  matrix1 <- matrix1[-no_NA_index,]

  #...und nicht absolut datierter Spalten (Alle NAs)
  NA_index <- which(is.na(matrix1[1,2:ncol(matrix1)]))+1   #gibt Vektor mit nicht absolut datierten Spalten an.
  #2:ncol, da die erste Spalte die Datierungen enthaelt.
  #"+1", da wegen 2:ncol die zweite Spalte als erste gez?hlte wird etc.
  matrix1 <- matrix1[,-NA_index]


  analysis_functions_objects()

  return(matrix1)

}#end function



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- FUNKTION erstelle_tabelle --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

erstelle_tabelle <- function(matrix_ueber, matrix_unter, liste_daten_gesamt)
{

  # Deklarieren der Tabelle
  tabelle_dat <- as.data.frame(matrix(data=NA, nrow = nrow(matrix_ueber)-1, ncol = 4))# -1, da die erste Zeile die Datierungen enthaelt
  tabelle_dat[,1] <- row.names(matrix_ueber[2:nrow(matrix_ueber),])# fuegt die Zeilennamen in die erste Spalte ein, bis auf die erste
  colnames(tabelle_dat) <- c("Stelle", "absolut", "j?nger/gleich", "?lter/gleich")

  #--- --- --- 1. Bef?llen mit rel. Datierungen --- --- ---
  # nimmt die letzte belegte Position aus den Zeilen der matrix_ueber und f?gt diese bei juenger/gleich ein
  # erzeugt eine matrix, an die die relationen der absoluten Datierungen angereiht werden k?nnen
  date_relation <- matrix(c("Befund", "datiert den Befund","auf das Datum juenger/gleich"), nrow = 1, ncol = 3)

  for(zeile in 2:nrow(matrix_ueber))
  {
    pos2 <- which(matrix_ueber[zeile,] >0)# pos2[length(pos2)] gibt die letzte belegte Position in der Zeile an

    if(length(pos2) != 0)# prueft ob die Zeile belegt ist, da sonst ein Fehler eintritt
    {
      tabelle_dat[zeile-1,3] <- matrix_ueber[1,pos2[length(pos2)]]# bei der Zeilenansprache der tabelle_dat muss wiederum 1 abgezogen werden!

      date_relation_temp <- c(colnames(matrix_ueber)[pos2[length(pos2)]], tabelle_dat[zeile-1,1], matrix_ueber[1,pos2[length(pos2)]])
      date_relation <- rbind(date_relation, date_relation_temp)

    }# Ende if-loop
  }# Ende for-loop


  #Anhaengen der aelter/gleich Beziehungen
  date_relation <- rbind(date_relation, c("Befund", "datiert den Befund","auf das Datum aelter/gleich"))

  # nimmt die als erste belegte Position aus den Zeilen der matrix_unter und f?gt diese bei aelter/gleich ein
  for(zeile in 2:nrow(matrix_unter))# ab Zeile 2, da Zeile 1 die Datierungen enth?lt
  {
    pos <- which(matrix_unter[zeile,] >0)# pos[1] gibt die erste belegte Position in der Zeile an

    if(length(pos) != 0)# prueft ob die Zeile belegt ist, da sonst ein Fehler eintritt
    {
    tabelle_dat[zeile-1,4] <- matrix_unter[1,pos[1]]# bei der Zeilenansprache der tabelle_dat muss wiederum 1 abgezogen werden!

    date_relation_temp <- c(colnames(matrix_ueber)[pos[1]], tabelle_dat[zeile-1,1], matrix_unter[1,pos[1]])
    date_relation <- rbind(date_relation, date_relation_temp)

    }
  }# Ende for-loop
  # write.table(date_relation, file="C:\\Users\\Johnny B.\ Goode\\Desktop\\R\\2017_02_27\\Output\\date_relation.csv", row.names=FALSE, col.names=FALSE, sep = " , ")

  #--- --- --- 2. Ansetzen der absolut datierten Stellen --- --- ---

  # L?schen der zuvor nicht datierten Stellen aus der liste_daten_gesamt
  liste_daten_gesamt_bereinigt <- liste_daten_gesamt
  NA_index <- which(is.na(liste_daten_gesamt[,2]))# zeigt an, welche Zeilen ein NA enthalten
  liste_daten_gesamt_bereinigt <- liste_daten_gesamt_bereinigt[-NA_index,]# loescht besagte Zeilen

  # Deklarieren und bef?llen der finalen Tabelle
  liste_daten_gesamt_bereinigt <- cbind(liste_daten_gesamt_bereinigt, NA, NA)
  colnames(liste_daten_gesamt_bereinigt) <- c("Stelle", "absolut", "j?nger/gleich", "?lter/gleich")
  tabelle_final <- rbind(tabelle_dat,liste_daten_gesamt_bereinigt)


  # Sortieren der Tabelle
  tabelle_final <- tabelle_final[order(tabelle_final[,3], decreasing = TRUE),]

  analysis_functions_objects()

  return(tabelle_final)

}# end function

# --- --- --- --- --- FUNCTION analysis_functions_objects --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---
analysis_functions_objects<- function()
{
  analyse = TRUE
  # analyse = FALSE

  if (analyse == TRUE)
  {
    name_of_function <- sys.call(which = -1)
    name_of_calling_function <- sys.call(which = -2)

    calling_environment <- parent.frame()
    objects_in_function <- ls(calling_environment)

    if (!exists("list_functions_and_objects"))
    {
      list_functions_and_objects <<- list(name_of_calling_function, name_of_function, objects_in_function)
      names(list_functions_and_objects) <<- c("calling function", "function", "objects")
    }

    list_functions_and_objects <<- rbind(list_functions_and_objects, list(name_of_calling_function, name_of_function, objects_in_function))
    list_functions_and_objects <<- unique(list_functions_and_objects)
  }
}
