MONO=mono
COMPILER=fsharpc
FILES=Helpers.fs ErrorMonad.fs Mail.fs Contact.fs Program.fs
REFERENCES=-r /usr/lib/monogame/Lidgren.Network.dll

app.exe: ${FILES}
	${COMPILER} --nologo --target:exe --platform:x86 ${REFERENCES} -o $@ ${FILES}
