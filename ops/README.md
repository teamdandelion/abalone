You've just stumbled across the infrastructure utilities for the AI server.
Welcome!

# install ansible

Remote infrastructure is managed using `ansible`. If you don't have ansible, please
install it.

There are a few different ways to install it.

* `brew install ansible` is probably the simplest method.
* install from source code if you're willing to do a bit of extra work. the
  source installation often has useful features long before the distributions,
  so the juice is worth the squeeze. Not to worry though, we don't use any new
  features here (yet).

# getting started

This directory is organized into `roles` and `playbooks`.

**roles**

Roles allow us to express how servers ought to be configured. When we spin up a
new server, we assign it some roles and execute a task to transform it to its
configured state. Role execution is (for all intents and purposes) idempotent.
It's safe to execute roles more than once. Roles can be found in the `roles`
directory. See the `make configure_servers` task for more details.

Pedantry alert: In reality, a role is just another way to organize a playbook.
Nonetheless, in practice, it is useful to make the conceptual distinction
between roles and playbooks.

**playbooks**

Playbooks allow us to execute one-off tasks. For instance, we use playbooks to
deploy new versions of code. The effects of playbook runs will differ from
invocation to invocation. It's easier to manage infrastructure when these
dynamic actions are separated from the static role definitions. Playbooks can
be found in the `playbooks` directory.

**tasks**

Tasks, defined in the `Makefile`, describe operations that we wish to perform
on remote servers. We use tasks to configure servers, perform status checks,
deploy releases, etc. See the `Makefile` for details.
