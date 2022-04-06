# Mithril adversarial events

This documents serves as a log of different adversarial events that we think of, and that we wish to 
include in our testing framework. 

Network attacks: 
- [ ] Partitions
- [ ] Delays
- [ ] Reordering of packages
- [ ] Isolation of honest nodes

Protocol attacks: 
- [ ] Sending invalid signatures
- [ ] Invalidation of signature if adversary did not get "satisfactory" result of the election
- [ ] Test the claims made in the paper. What portion of the stake does the adversary need to break the system.
  Ideally, this would be combined with network attacks, where the adversary, controlling a smaller portion that 
  the expected threshold, isolates honest nodes, or disqualifies them somehow.
- [ ] Improve testing framework of rust library with more engaged attacks.