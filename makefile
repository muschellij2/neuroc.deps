all:
	wget -O inst/neuroc_appveyor.yml https://raw.githubusercontent.com/muschellij2/neuroc_travis/master/neuroc_appveyor.yml 
	wget -O inst/neuroc_travis_ants.yml  https://raw.githubusercontent.com/muschellij2/neuroc_travis/master/neuroc_travis_ants.yml 
	wget -O inst/neuroc_travis.yml  https://raw.githubusercontent.com/muschellij2/neuroc_travis/master/neuroc_travis.yml 

clean: 
	rm -f inst/*.yml
