#!/bin/bash

vagrant up default
vagrant ssh default -c 'sudo bash -c "cd /vagrant/src && dpkg-buildpackage && mv ../*.deb . && chown vagrant: *.deb"'
vagrant halt default
