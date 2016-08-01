# schedulous
Schedule workers to timeslots with ease.

# purpose

Schedulous is intended to make scheduling easier.  You provide a set of available workers and a set of timeslots to Schedulous (both in CSV format), along with a set of constraints you want met, and Schedulous finds a schedule for you.

# prerequisites

You will need the following software installed:

1. Scala (I use 2.11.7)
2. SBT (I use 0.13.9)
3. Z3 (I use 4.4.1)

# how to use it

After you have installed the above prerequisites, you will need to create two CSVs.

1. You will need a `events.csv` file containing all of the timeslots you need filled, one per line.  It must have the at least the following header fields in any order: `date,start,end,duration,event,role,approval,person`.  Extra header fields will be ignored.
2. You will need a `workers.csv' file containing all of the workers and their availability, one worker per line.  It must have at least the following header fields in any order: `First Name,Last Name` as well as a set of fields indicating availability, e.g., `Available Tue 6/14`.
3. For now, you will need to create/modify the `VolunteerCSVReader` to indicate how to interpret the ``Available [date]` fields.  See [here](https://github.com/dbarowy/schedulous/blob/master/src/main/scala/Readers/VolunteerCSVReader.scala).
4. Create a Scala program that reads in your event and worker data files, sets Schedulous configuration options, and then produces a schedule.  Feel free to modify the example [here](https://github.com/dbarowy/schedulous/blob/master/src/main/scala/SchedulousDemoApp.scala).
5. Run your program.

# how it works

Schedulous is built on top of the Z3 constraint solver.  Constraints are implemented in a simple SMTLIB DSL written in Scala.  You can find a set of sample constraints in the `src/main/scala/Constraints` folder.  You can add constraints by implementing [this](https://github.com/dbarowy/schedulous/blob/master/src/main/scala/Constraints/Constraint.scala) trait and then by adding them to your Schedulous configuration.

The set of currently available constraints are:

1. Ensure a "fair" workload.  All workers work the same number of hours +/- `e` of minutes (user-definable).
2. Ensure that all timeslots are filled.
3. Limit the total number of days that a worker works.
4. Ensure that workers have at most `n` assignments (user-definable).
5. Ensure that workers have at least `n` assignments (user-definable).
6. Ensure that workers only work 1 assignment at a time (i.e., no concurrent assignments).

# contributions

Schedulous is currently pretty rough around the edges, but it works.  If you have modifications that help you---even trivial ones---I'd be happy to accept them.  I am particularly interested in changes that make it easy to run this without having to modify source files.
