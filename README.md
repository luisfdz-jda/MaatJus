# [Maat.Jus](https://es.wikipedia.org/wiki/Maat)

**M**ecanismo **a**ndaluz de **a**cceso al **t**eletrabajo en órganos judiciales y oficinas fiscales

Consejería de Turismo, Regeneración, Justicia y Administración Local

## Descargas
Windows 64 bits: [maat.exe](https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.exe)

# Cambios de versión de una release
- Editar la release y sustituir el fichero correspondiente por el fichero con la nueva versión (nuevo fichero de haxm, qemu o imagen iso)
- Modificar el contenido del fichero maat_online_version.json cambiando el número de version del nuevo componente de Maat.
- La siguiente vez que el usuario ejecute maat.exe se detectará el cambio de versión y se instalaran los nuevos componentes de manera transparente para el usuario

## win32 x86_64 release

[https://github.com/luisfdz-jda/MaatJus/releases/tag/maat_win32_x86_64](https://github.com/luisfdz-jda/MaatJus/releases/tag/maat_win32_x86_64)

### Ficheros creados

    %TEMP%\haxm.tgz
    %TEMP%qemu.tgz

    %LOCALAPPDATA%\haxm
    %LOCALAPPDATA%\qemu
    %LOCALAPPDATA%\qemu\qemu_launcher.bat
    %LOCALAPPDATA%\maat_online_version.json
    %LOCALAPPDATA%\maat_installed_version.json
    %LOCALAPPDATA%\maat.iso
    %LOCALAPPDATA%\maat.exe
    %LOCALAPPDATA%\maat1.exe (cuando haya cambio de version de maat.exe)
    %LOCALAPPDATA%\maat.bat
    %LOCALAPPDATA%\maat.ico
    %LOCALAPPDATA%\uninstall_maat.bat

    %USERPROFILE%\Downloads\maat.exe
    %USERPROFILE%\Desktop\Maat.Jus.lnk

## macOS
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
brew install qemu