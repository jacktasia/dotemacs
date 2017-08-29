# -*- mode: ruby -*-
# vi: set ft=ruby :

# This configuration requires Vagrant 1.5 or newer
#   Plugins:
#   vagrant plugin install vagrant-hosts        ~> 2.1.4
#   vagrant plugin install vagrant-auto_network ~> 1.0.0
#   vagrant plugin install vagrant-triggers

Vagrant.require_version ">= 1.5.0"

require 'vagrant-hosts'
require 'vagrant-auto_network'
require 'vagrant-triggers'


Vagrant.configure('2') do |config|

  config.vm.define :emacsdebug do |node|
    node.vm.box = 'ubuntu/xenial64'

    node.vm.hostname = 'emacsdebug.vlan'

    # Use vagrant-auto_network to assign an IP address.
    node.vm.network :private_network, :auto_network => true

    # Use vagrant-hosts to add entries to /etc/hosts for each virtual machine
    # in this file.
    node.vm.provision :hosts

    node.vm.provision :shell, :path => 'provision.sh'

    node.vm.provider "virtualbox" do |v|
      v.memory = 768
      v.cpus = 1
    end
  end

  copy_key_command = "cp %s/.ssh/id_rsa ./id_rsa" % [ENV['HOME']]
  config.trigger.before :up do
    info "Copying ~/.ssh/id_rsa into this directory so github can load it for cloning emacs config"
    run copy_key_command
  end

end
