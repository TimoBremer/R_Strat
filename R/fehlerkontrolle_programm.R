# Funktion ausgabe_widerspruchskette---------------------------------------------------
ausgabe_widerspruchskette <- function(bisherige_widerspruchskette, ergaenzung) {
    trennlinie_schichten <- c("......", "......", "......")
    widerspruchskette <- rbind(ergaenzung, trennlinie_schichten, bisherige_widerspruchskette)
    return(widerspruchskette)
}

# Funktion Suche Fehlerkette------------------------------------------------------------
suche_fehlerkette <- function(gesuchte_stelle) {
    print(paste("gesuchte Stelle:", gesuchte_stelle))
    ## Wenn in der Funktion 'Widerspruchsanalyse' ein Fehler aufgetreten ist, wird diese Funktion aufgerufen verfolgt die Fehlerkette in den
    ## urspruenglichen Daten ('liste_alle_beziehungen') ausgehend von der Stelle, in der der Fehler festgestellt wurde

    liste_alle_beziehungen <- rbind(liste_ueber_temp, liste_gleich_temp)

    # fuer den ersten Durchlauf ist die Eingangsliste gleich der Restliste:
    liste_rest <- liste_alle_beziehungen
    listennummer <- 1
    # fuer den initialen Durchlauf ist die Abfrage gleich der gesuchten Stelle
    abfrage <- gesuchte_stelle

    # Schleife Abfrage Hinweg----------------- in dieser Schleife werden alle Beziehungen zur gesuchten Stelle in einzelne Listen geschrieben
    # ...bis die gesuchte Stelle wieder auftaucht, d.h. eine Schleife und somit ein Widerspruch gefunden wurde (1....1).  ...Die Listen sind
    # dabei hierarchisch: die Beziehungen in 'liste_3' sind daher drei Beziehungen von der gesuchten Stelle entfernt ...es ergibt sich somit
    # eine hierarchische Datenbankstruktur
    repeat {
        # zuvor getrennte Listen werden wieder verbunden (s.u.): ...'liste_rest_gleich' gibt es erst nach erstem Durchgang ...Verbindung ergibt erst
        # in drittem Durchgang Sinn
        if (listennummer > 2) {
            liste_rest <- rbind(liste_rest, liste_rest_gleich_2)
        }
        # alle Datensaetze abfragen, die gesuchtes Attribut enthalten:
        abgefragte_liste <- subset(liste_rest, liste_rest[, 1] %in% abfrage)

        # Liste mit restlichen Beziehungen erstellen, denn einmal verwendete Beziehungen werden (wahrscheinlich) nicht mehr gebraucht:
        liste_rest <- subset(liste_rest, !(liste_rest[, 1] %in% abfrage))

        ## Gegenwerte bei gleich loeschen: zuerst eine Abfrage mit allen Gleich-Werten, die bei diesem Durchlauf verwendet wurden:
        abgefragte_liste_gleich <- subset(abgefragte_liste, abgefragte_liste[, 2] == "gleich")
        # rechte Seite der abgefragten Gleich-Beziehungen:
        abgefragte_liste_gleich <- array(abgefragte_liste_gleich[, 3])

        # beim zweiten Durchgang muessen die gesicherten Gleich-Beziehungen verschoben werden:
        if (listennummer > 1) {
            liste_rest_gleich_2 <- liste_rest_gleich
        }

        # Sichern der Gegenwerte fuer die uebernaechste Runde:
        liste_rest_gleich <- subset(liste_rest, liste_rest[, 1] %in% abgefragte_liste_gleich & liste_rest[, 2] == "gleich" & liste_rest[, 3] %in%
            abfrage)
        # Entfernen der Gegenwerte fuer eine Runde, bei 1 = 3 z.B. 3:
        liste_rest <- subset(liste_rest, !(liste_rest[, 1] %in% abgefragte_liste_gleich & liste_rest[, 2] == "gleich" & liste_rest[, 3] %in%
            abfrage))

        # nun die neue Abfrage erstellen:
        abfrage <- array(abgefragte_liste[, 3])



        # fortlaufende Liste mit Fehlerkette erstellen----------- mit 'assign' werden aus der letzten abgefragten Liste zusammen mit der
        # Listennummer fortlaufende Listen erstellt (liste_1, liste_2 usw.):
        assign(paste("liste_", listennummer, sep = ""), abgefragte_liste)
        listennummer <- listennummer + 1

        # pruefen, ob Weg zu gesuchter Stelle gefunde wurde: hier fehlt noch irgendeine Art error.handler, damit die Schleife bei einem Fehler nicht
        # unendlich weiterlaeuft!!!
        if (gesuchte_stelle %in% abfrage)
            (break)()
        # Sicherung gegen Endlossschleife: wenn mehr Durchlaeufe als Beziehungen:
        if (listennummer > nrow(liste_alle_beziehungen)) {
            stop("das Programm konnte keinen Widerspruch in der Stratigrafie ermitteln, was auf einen internen Fehler zurueckzufuehren ist!")
        }
    }


    # Vorbereitung Rueckweg von Fehler zu Ausgangspunkt------------------ da die Listen bislang alle Beziehungen enthalten die von der
    # Fehlerursache ausgehen, muss diese nun rueckwaerts durchlaufen werden ...damit am Ende ein einziger Pfad als Fehlerkette uebrig bleibt
    # ...entspricht im Grunde einer Datenbankabfrage der 'gesuchten_stelle' ueber alle Tabelle (z. B. liste_5, liste_4...)  ...aehnelt sehr stark
    # der obigen Schleife

    # zunaechst muss die listennummer um 2 herabgesetzt werden, da im letzten Schritt in der obigen Schleife keine Liste mehr angelegt wurde:
    listennummer <- listennummer - 1

    # Abfrage wird wieder gleich gesuchte Stelle gesetzt:
    abfrage <- gesuchte_stelle

    # Tabelle Widerspruchskette muss vorab definiert werden: muss nacher noch schoener geloest werden, ist schließlich die Fehlerausgabe fuer den
    # Benutzer!!!
    widerspruchskette <- matrix(data = 0, nrow = 0, ncol = 3)
    # Schleife Rueckweg------------ jetzt wird die Listennummer rueckwaerts gezaehlt
    for (listennummer in listennummer:1) {
        # aus der Listennummer wird die aktuelle Liste erstellt:
        abgefragte_liste <- eval(as.name(paste("liste_", listennummer, sep = "")))
        abgefragte_liste <- subset(abgefragte_liste, abgefragte_liste[, 3] %in% abfrage)
        # und wieder mit 'assign' wird die 'abgefragte_liste' erneut an die entsprechende Liste 'liste_1', 'liste_2' etc. uebergeben:
        assign(paste("liste_", listennummer, sep = ""), abgefragte_liste)
        # hiermit wird der Wert fuer die naechste Abfrage im folgenden Durchlauf geholt:
        abfrage <- array(abgefragte_liste[, 1])

        widerspruchskette <- ausgabe_widerspruchskette(widerspruchskette, abgefragte_liste)
    }
    return(widerspruchskette)
    ### _____________________________________ Einzellisten koennte man in große Liste zusammenfuehren, weiß aber noch nicht, wozu das nuetzlich ist
    ### kann vielleicht mehr Listen aufnehmen oder ist schneller?
}
