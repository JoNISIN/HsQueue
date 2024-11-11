# An Implementation of Queue in Haskell

For haskell, its list data structure works as stack generally. 

If we operate the list as queue, it need to cost O(n) calculation complexity to find the head, and use O(n) calculation complexity to append an element. For this reasion, the total calculation complexity to operate the list as queue may be approximated to O(n^2).

Here is a sample to implement the queue data structure with O(n) in Haskell.
