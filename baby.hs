doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x
doubleSmallNumber' x = doubleSmallNumber x + 1
conanO'Brien = "It's a-me, Conan O'Brien!"  