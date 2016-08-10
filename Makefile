
all : 
	fsharpc Parse.fs LangParser.fs -o out.exe

clean : 
	rm *.exe
