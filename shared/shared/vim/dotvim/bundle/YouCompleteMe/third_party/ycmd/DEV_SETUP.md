# Setting up for ycmd development

1. Install [Vagrant][].
2. `cd` into the folder where you checked out ycmd.
3. `$ vagrant up && vagrant ssh`. This will take a while because the VM is being
	 built and set up. Only needs to happen once though.
4. You are now in the VM. Run the tests with `$ ./run_tests.sh`.
5. Hack away. When done, exit the ssh connection with `exit`.
6. `$ vagrant suspend` so that you can quickly get back to hacking later.
7. Later on: `$ vagrant resume && vagrant ssh`. This will be _much_ faster.

That's it!

You can switch between Python versions with `pyenv global 2.6.6` and `pyenv
global 3.3.0`.

If you ever feel like you've screwed up the VM, just kill it with
`vagrant destroy` and then run `vagrant up` again to get to a clean state.

[vagrant]: https://www.vagrantup.com/
