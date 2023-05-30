# Brainfuck interpreter
Tento program je interpret ezoterického programovacieho jazyka Brianfuck implementovaný v jazyku Haskell. Referencia: http://brainfuck.org/brainfuck.html

Základná veľkosť pásky je 30000. Veľkosť jedného políčka je 8 bitov. Môže nastať overflow/underflow.

## Kompilácia
Program skompilujte v príkazovej riadke pomocou kompilátora `ghc` príkazom
```bash
ghc brainfuck.hs
```

## Spúšťanie cez príkazovú riadku
Skompilovaný program je možné spustiť s parametrom názvu súboru s Brainfuck zdrojovým kódom. Je možné pridať ďalší parameter, ktorý udáva veľkosť pásky. Ak nie je druhý parameter špecifikovaný, tak veľkosť pásky je 30000.
```bash
./brainfuck example.bf
```
V prípade, že daný súbor neobsahuje validný zdrojový kód, program vyhodí výnimku. Znaky ktoré nie sú Brainfuck inštrukcie ('+', '-', '<', '>', '.', ',', '[', ']') sú ignorované. Za validný kód sa považuje súbor v ktorom sú znaky '[' a ']' správne uzátvorkované.

## Interaktívny režim
Cez `ghci` je možné s programom pracovať interaktívne. Zaujímavé funkcie sú napríklad: 
```hs
runFile :: String -> IO ()
```
Toto je funkcia, ktorú volá aj `main` funkcia. Načíta zdroják zo súboru a pokúsi sa ho spustiť. Používa stdin/stdout.

```hs
run :: String -> IO ()
```
Parameter je priamo string so zdrojovým kódom.

```hs
parseSourceCode :: String -> Program
```
Vráti zoznam inštrukcií.

```hs
debugPrint :: IO Tape -> Int -> IO ()
```
Parametre sú páska a počet prvkov okolo pointera, ktoré vypíše na stdout.

## Príklady
Zložka `examples` obsahuje niekoľko ukážkových Brainfuck programov.
Príklady:

 - `examples/numwarp.bf` užívateľ zadá číslo a po stlačení Enter sa vypíše číslo diagonálne. Ukážka:
    ``` bash
    $ ./brainfuck examples/numwarp.bf
    12345
           /
           \/\
          \  /
         \/\
       /\
        /\
     /\  /
      /
    \ \/
     \
    ```
 - `examples/hello.bf` Klasický hello world program
 - `examples/fib.bf` Vypisuje fibonacciho postupnosť. Program sám nezastaví, je potrebné ho ukončiť pomocou CTRL+C.
 - `examples/calculator.bf` Kalkulačka. Vstupy sú tvaru napr. `4*5=`. Hodnoty môžu pretiecť, keďže presnosť bunky je len 8 bitov. (Viac info v zdrojáku.)
 - `examples/life.bf` Game of life. Užívateľ postupne zadáva príkazy. Príkaz so súradnicami políčka (+Enter) nastaví danú bunku na živú ak bola predtým mŕtva alebo opačne. Prázdny riadok (len Enter) spraví jednu iteráciu hry. Príkaz `q` ukončí program. Mapa je torus, takže horná časť a spodná časť sú spojené a rovnako pravá s ľavou časťou.
 - `examples/squares.bf` Vypíše druhé mocniny čísel menšie ako 10000.
 - `examples/tictactoe.bf` Hra tic-tac-toe proti AI. Užívateľ zadá číslo políčka kam chce zahrať a po nejakom čase zahrá počítač.
 - `examples/triangle.bf` Vykreslí Sierpińského trojúholník.

Poznámka: Autori programov a bližšie inštrukcie a popis niektorých programov sú priamo v zdrojových kódoch.
