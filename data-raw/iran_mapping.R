library(data.table)

# historical and present day locations that map directly to Iran
iran_mapping1 <- data.table(parent = "Iran",
                            child = c("Markazi 1956", "Gilan 1956-1966",
                                      "Mazandaran 1956-1996",
                                      "East Azarbayejan 1956-1986",
                                      "West Azarbayejan",
                                      "Kermanshahan 1956",
                                      "Kurdistan",
                                      "Khuzestan and Lorestan 1956",
                                      "Fars and Ports 1956",
                                      "Kerman",
                                      "Khorasan 1956-1996",
                                      "Isfahan and Yazd 1956",
                                      "Sistan and Baluchistan"))

# mappings for highest level of historical locations
iran_mapping2.1 <- data.table(parent = "Markazi 1956",
                              child = c("Markazi 1966-1976", "Semnan"))
iran_mapping2.2 <- data.table(parent = "Gilan 1956-1966",
                              child = c("Gilan", "Zanjan 1976-1996"))
iran_mapping2.3 <- data.table(parent = "Mazandaran 1956-1996",
                              child = c("Mazandaran", "Golestan"))
iran_mapping2.4 <- data.table(parent = "East Azarbayejan 1956-1986",
                              child = c("East Azarbayejan", "Ardebil"))
iran_mapping2.5 <- data.table(parent = "Kermanshahan 1956",
                              child = c("Ilam", "Kermanshah", "Hamadan"))
iran_mapping2.6 <- data.table(parent = "Khuzestan and Lorestan 1956",
                              child = c("Khuzestan", "Lorestan"))
iran_mapping2.7 <- data.table(parent = "Fars and Ports 1956",
                              child = c("Fars", "Hormozgan", "Bushehr",
                                        "Kohgiluyeh and Boyer-Ahmad"))
iran_mapping2.8 <- data.table(parent = "Khorasan 1956-1996",
                              child = c("Khorasan-e-Razavi", "North Khorasan",
                                        "South Khorasan"))
iran_mapping2.9 <- data.table(parent = "Isfahan and Yazd 1956",
                              child = c("Isfahan and Yazd 1966",
                                        "Chahar Mahaal and Bakhtiari"))

# mappings for next level of historical locations
iran_mapping3.1 <- data.table(parent = "Markazi 1966-1976",
                              child = c("Markazi", "Tehran 1986"))
iran_mapping3.2 <- data.table(parent = "Zanjan 1976-1996",
                              child = c("Zanjan", "Qazvin"))
iran_mapping3.3 <- data.table(parent = "Isfahan and Yazd 1966",
                              child = c("Isfahan", "Yazd"))

# mappings for next level of historical locations
iran_mapping4 <- data.table(parent = "Tehran 1986",
                            child = c("Tehran 1996-2006", "Qom"))

# mappings for next level of historical locations
iran_mapping5 <- data.table(parent = "Tehran 1996-2006",
                            child = c("Tehran", "Alborz"))

# combine all together
iran_mapping <- rbind(iran_mapping1,
                      iran_mapping2.1, iran_mapping2.2,
                      iran_mapping2.3, iran_mapping2.4, iran_mapping2.5,
                      iran_mapping2.6, iran_mapping2.7, iran_mapping2.8,
                      iran_mapping2.9,
                      iran_mapping3.1, iran_mapping3.2, iran_mapping3.3,
                      iran_mapping4, iran_mapping5,
                      use.names = T)

usethis::use_data(iran_mapping, overwrite = TRUE)
