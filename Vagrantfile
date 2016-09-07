# -*- mode: ruby -*- vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box_check_update = false

  config.vm.define "default" do |default|
    default.vm.hostname = "modxtransform"

    default.vm.provider "lxc"
    default.vm.box = "fgrehm/trusty64-lxc"

    # dpkg-buildpackage insists on putting the resulting deb in the directory
    # above the sources.
    default.vm.synced_folder ".", "/vagrant/src"

    default.vm.provision "shell", :inline => "export DEBIAN_FRONTEND=noninteractive; echo 'deb http://archive.ubuntu.com/ubuntu xenial main restricted universe multiverse' >> /etc/apt/sources.list; echo 'APT::Default-Release \"trusty\";' > /etc/apt/apt.conf.d/01ubuntu; apt-get update; apt-get -y install devscripts equivs; mk-build-deps -i /vagrant/src/debian/control -t 'apt-get --no-install-recommends -y'; apt-get -y install sbcl -t n=xenial"
  end
end
