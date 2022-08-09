intérêt montant taux mois = montant * (1 + taux / 100)**(mois/12) - montant

solde m t n r = let i = (1 + t / 100)**(1/12)
                in (m * i**n) - r * (1-i**n) / (1-i)

main :: IO ()
main = putStrLn "Finance"

