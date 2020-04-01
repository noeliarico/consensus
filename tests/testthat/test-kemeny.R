test_that("Kemeny1", {
  
  # Table 2.4
  por <- parse_profile_of_rankings("6, a ≻ b ≻ c ≻ d,
                                    5, b ≻ c ≻ a ≻ d,
                                    3, c ≻ d ≻ a ≻ b")
  #expect_equal(kemeny(por), parse_ranking("a ≻ b ≻ c ≻ d"))

})

# Copia y pega lo de arriba y escríbeme algunos profiles of rankings y lo q sale
# Solo tienes que cambiar las partes en azul
# El que hay es el que yo había utilizado inicialmente que aparece en tu tesis,
# pero ahora que lo vamos a utilizar hay que testearlo bien.
# El método no está definido para cuando hay empates, qué se hace en ese caso?
# En lo que escribas: utiliza ~ para los empates y ≻ o > para expresar que un
# candidato está en mejor posición que otro
# Puedes utilizar los nombres de candidatos que tú quieras.