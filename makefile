all:
	wget -O inst/neuroconductor_appveyor.yml https://raw.githubusercontent.com/muschellij2/neuroc_travis/master/neuroconductor_appveyor.yml 
	wget -O inst/neuroconductor_appveyor_ants.yml https://raw.githubusercontent.com/muschellij2/neuroc_travis/master/neuroconductor_appveyor_ants.yml 
	wget -O inst/neuroconductor_travis_ants.yml  https://raw.githubusercontent.com/muschellij2/neuroc_travis/master/neuroconductor_travis_ants.yml 
	wget -O inst/neuroconductor_travis.yml  https://raw.githubusercontent.com/muschellij2/neuroc_travis/master/neuroconductor_travis.yml 

clean: 
	rm -f inst/*.yml
