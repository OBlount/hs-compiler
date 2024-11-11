@echo off

ghc -isrc Main.hs -o mtc
for /r %%f in (*.o) do del "%%f"
for /r %%f in (*.hi) do del "%%f"
