# Haskell_PvZ
Egyetemi tanulmányaim első félévében egy beadandó feladat során a Haskell nyelvvel kellett egy szöveges Plants vs. Zombies szimulációt készítenünk. A cél a rekurzió, az adattípusok és a funkcionális programozás alkalmazása volt, miközben a játék mechanizmusait modelleztük, például növények elhelyezését és zombik mozgását a pályán.


# Feladat leírása
A beadandó során a Plants vs. Zombies játék egyszerűsített szimulációját kellett megvalósítanunk. A játék alapvető szabályai a következők voltak:

Egy 5 sorból és 12 oszlopból álló pályán játszódik.
A zombik jobbról balra haladnak, és ha elérik a pálya bal oldalát, a játékos elveszíti a játékot.
A játékos növényeket helyezhet el a pályán, amelyek védelmi funkciót látnak el.
A játék célja a zombik megállítása, mielőtt elérnék a pálya bal szélét.
A játék szimulációja diszkrét körökre oszlik, ahol minden kör végén az összes növény és zombi végrehajtja a rájuk jellemző akciókat, például a növények lőnek, a zombik pedig előrehaladnak vagy támadnak.
