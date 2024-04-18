
.PHONY: ubuntu plumed2.tgz clean fedora40 fedora40-pycv rocky8 rocky8-pycv 

ubuntu: plumed2.tgz
	docker build --shm-size 1024M -t plumed --progress=plain .

fedora40: plumed2.tgz
	docker build --shm-size 1024M -t plumed:fedora40 --progress=plain -f fedora40 .
fedora40-pycv: plumed2.tgz
	docker build --shm-size 1024M -t plumed:fedora40-pycv --progress=plain -f fedora40-pycv .


rocky8: plumed2.tgz
	docker build --shm-size 1024M -t plumed:rocky8 --progress=plain -f rocky8 .

rocky8-pycv: plumed2.tgz
	docker build --shm-size 1024M -t plumed:rocky8-pycv --progress=plain -f rocky8-pycv .

plumed2.tgz:
	cd ../ ; git archive HEAD -o "$(CURDIR)"/plumed2.tgz --prefix plumed2/ ; cd -

clean:
	rm -f plumed2.tgz
