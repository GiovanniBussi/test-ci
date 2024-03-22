
.PHONY: ubuntu plumed2.tgz clean fedora39 fedora39-pycv rocky8 rocky8-pycv centos7

ubuntu: plumed2.tgz
	docker build --shm-size 1024M -t plumed --progress=plain .

fedora39: plumed2.tgz
	docker build --shm-size 1024M -t plumed:fedora39 --progress=plain -f fedora39 .
fedora39-pycv: plumed2.tgz
	docker build --shm-size 1024M -t plumed:fedora39-pycv --progress=plain -f fedora39-pycv .


rocky8: plumed2.tgz
	docker build --shm-size 1024M -t plumed:rocky8 --progress=plain -f rocky8 .

rocky8-pycv: plumed2.tgz
	docker build --shm-size 1024M -t plumed:rocky8-pycv --progress=plain -f rocky8-pycv .

plumed2.tgz:
	cd ../ ; git archive HEAD -o "$(CURDIR)"/plumed2.tgz --prefix plumed2/ ; cd -

centos7: plumed2.tgz
	docker build --shm-size 1024M -t plumed:centos7 --progress=plain -f centos7 .
clean:
	rm -f plumed2.tgz
