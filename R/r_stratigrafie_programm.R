RStratigraphy <- function(liste_stratigrafie, liste_absolute_datierungen, fehler_loeschen)
{
Änderung
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- Formatierung der Daten fuer Weiterverarbeitung --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

# Liste in Typ "character" umwandeln, da numerische und nicht numerische Befundbezeichnungen sonst nicht gleich behandelt werden:
liste_stratigrafie[] <- lapply(liste_stratigrafie, as.character)

# Erstellen einzelner Listen nach Lageverhaeltnis der Befunde untereinander:
# Die Temp-Tabellen werden von der Fehlerkontrolle verwendet!
liste_ueber_temp <- subset(liste_stratigrafie, liste_stratigrafie[,2] == "ueber")
liste_unter_temp <- subset(liste_stratigrafie, liste_stratigrafie[,2] == "unter")
liste_gleich_temp <- subset(liste_stratigrafie, liste_stratigrafie[,2] == "gleich")

# Listen vervollstaendigen und fuer die Befuellung der Matrix vorbereiten
# Unter-Liste in Ueber-Liste uebersetzen und mit bestehender ueber-Liste zusammenfuehren:
liste_ueber_temp <- ergaenzen_listen(liste_ueber_temp, liste_unter_temp)
# in Gleich-Liste werden umgekehrte Ausdruecke eingetragen (3 = 5 und 5 = 3):
liste_gleich_temp <- ergaenzen_listen(liste_gleich_temp)

# Spalte mit über/unter bzw. gleich löschen:
liste_ueber <- liste_ueber_temp[,c(1,3)]
liste_gleich <- liste_gleich_temp[,c(1,3)]

# Liste mit allen beteiligten Stellen erstellen
# Liste Stellen wird aus Spalte links und Spalte rechts erstellt:
liste_stellen <- c(array(liste_stratigrafie[,1]),array(liste_stratigrafie[,3]))

# Sortieren der Stellen:
liste_stellen <- liste_stellen[order(liste_stellen)]

# Löschen doppelt genannte Stellen.
liste_stellen <- unique(liste_stellen)



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- Erstellen der Matrizen --- --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

# Erstellen der leeren Grundmatrix
# Erstellen einer Matrix befüllt mit Nullen, die so viel Zeilen und Spalten hat, wie Stellen (liste_stellen) vorhanden sind:
leere_matrix <- as.data.frame(matrix(data=0, nrow = length(liste_stellen), ncol = length(liste_stellen)))

# Stellenliste muss nun in Vektor umformatiert werden:
liste_stellen <- as.vector(liste_stellen)

# Benennung der Zeilen und Spalten der leeren Matrix
dimnames(leere_matrix) <- list(liste_stellen, liste_stellen)


# --- --- --- Befuellen der Matritzen --- --- ---

# 1. Matrix mit gleich-Werten befüllen:
matrix_gleich <<-befuellen_matrix(leere_matrix, liste_gleich)

# 2. Matrix mit über- und unter-Werten befüllen:
matrix_ueber_unter <<-befuellen_matrix(leere_matrix, liste_ueber)

# Iteration über alle Felder (zunächst alle Spalten einer Zeile, dann nächste Zeile):
# Widerspruchsanalyse an Ursprungsdaten ist erforderlich, um kurzkettige Fehler aufzuspüren
print(paste("Wiederspruchsanalyse l?uft..."))
for(zeilennr in 1:nrow(matrix_ueber_unter)) # Iteration Zeilen
{
  for(spaltennr in 1:ncol(matrix_ueber_unter)) # Iteration spalten
  {
    if (matrix_ueber_unter[zeilennr,spaltennr] == 1) # gucken ob Feld besetzt ist in Matrix über/unter
    {
      widerspruchsanalyse(zeilennr, spaltennr, TRUE, fehler_loeschen)
    }
  }
}



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- Vervollständigen der Matritzen --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---
durchlaeufe <-1 # für die Anzeige, damit man sieht, dass das Programm läuft
repeat
{
  summe_matrix_ueber_unter <- sum(rowSums(matrix_ueber_unter))
  summe_matrix_gleich <- sum(colSums(matrix_gleich))

  # Iteration über alle Felder (zunächst alle Spalten einer Zeile, dann nächste Zeile):
  for(zeilennr in 1:nrow(matrix_ueber_unter)) # Iteration Zeilen
  {
    for(spaltennr in 1:ncol(matrix_ueber_unter)) # Iteration spalten
    {
      if (matrix_gleich[zeilennr,spaltennr] == 1) # gucken ob Feld besetzt ist in Matrix gleich
      {
        matrix_gleich <- gleichsetzen_spalten(matrix_gleich, zeilennr, spaltennr) # Ergänzen Matrix gleich
        # falls die Diagonale besetzt wird (3=3) wird sie hiermit freigeräumt, ist nicht notwendig aber effizienter:
        matrix_gleich[zeilennr, zeilennr] <- 0
        # wenn Feld gleich werden Zeilen in Matrix über/unter gleichgesetzt:
        matrix_ueber_unter <- gleichsetzen_spalten(matrix_ueber_unter, zeilennr, spaltennr)
        # und auch die Spalten werden gleichgesetzt, daher Transponierung
        matrix_ueber_unter <- t(matrix_ueber_unter)
        matrix_ueber_unter <- gleichsetzen_spalten(matrix_ueber_unter, zeilennr, spaltennr) # Abgleichen Spalten
        matrix_ueber_unter <- t(matrix_ueber_unter) # Matrix wird wieder in Augangslage gekippt
      }

      if (matrix_ueber_unter[zeilennr,spaltennr] == 1) # gucken ob Feld besetzt ist in Matrix über/unter
      {
        # Abgleich der Daten innerhalb der "matrix_ueber_unter"
        matrix_ueber_unter <- gleichsetzen_spalten(matrix_ueber_unter, zeilennr, spaltennr) # Gleichsetzen Spalten in der über/unter Matrix
        # dürfte Fehler erst nach dem Gleichsetzen der Spalten erkennen, daher vielleicht eine Zeile höher? bei Zeiten ausprobieren!
        widerspruchsanalyse(zeilennr, spaltennr, FALSE, fehler_loeschen)
      }
    }
  } # Ende Schleife Zeilennr. = 1
  if (summe_matrix_ueber_unter == sum(rowSums(matrix_ueber_unter)) && summe_matrix_gleich == sum(colSums(matrix_gleich))) break()
  print(paste("        Durchlauf:" , durchlaeufe, "abgeschlossen"))
  durchlaeufe <- durchlaeufe +1

} # Ende Repeat-Schleife



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- Ergänzen absoluter Datierungen --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

final_tab <- absolute_datierungen(liste_absolute_datierungen, liste_stellen, matrix_gleich, matrix_ueber_unter)



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- Ausgabe der Daten --- --- --- --- --- --- ----

# --- --- --- --- --- --- --- --- --- --- --- ---

print(matrix_ueber_unter)
print(matrix_gleich)
#write.csv(matrix_ueber_unter, "/home/timo/Dropbox/Arbeitsfläche/stratigrafie_ueber_unter.csv")
#write.csv(matrix_gleich, "/home/timo/Dropbox/Arbeitsfläche/stratigrafie_gleich.csv")
#print(paste("Erfolg!!!, Anzahl Spalten Matrix-über-unter-etc.: ", sum(rowSums(matrix_ueber_unter)), "Matrix-gleich: ", sum(colSums(matrix_gleich))))
print(final_tab)
}

