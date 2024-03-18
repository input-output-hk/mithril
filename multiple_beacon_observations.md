# Multiple beacons types

Historically the `Beacon` type was created against the Cardano Db Snapshotting case (the first use case that we delivered).

But new signed entity types doesn't need its values, ie what's relevant for the future Cardano Transaction snapshots will
be block numbers (we will sign all blocks until a given block number every xxxx block numbers).

This means that we need to have a more generic Beacon types.

Since we use the legacy `Beacon` almost everywhere, and as it's core for our state machines, we need to approach this
issue carefully.

## Questions

* Does the certificate needs anything more than an epoch ?

  With multiple beacons types the concept of beacons will vary based on the signed entity type, as such putting a beacon
in the certificate will lose meaning (if a certificate have only an epoch as a beacon it could be either a Mithril Stake
Distribution or a Cardano Stake distribution, you won't know if you don't query the associated signed entity).

  Instead of putting a beacon in the certificate we can just use the existing signed entity type.

## Blockers

* **Major**: Our state machines use the beacon to know if they have to act, they also use it to retrieve or open a new OpenMessage.
   
  As such the state machines needs to have their own beacon type that contains every values that evolve with time that
we manipulate.
We could then convert this "global beacon" into specialized beacon depending on the current signed entity type that we
manipulate.

* **Minor**: The `CertificatePending` use a legacy beacon, but only the epoch of the beacon is used by the signers.
  
  We could keep it in the message for retro-compatibility (until an era?) and simplify our internal type to only have
the epoch.

* **Major**: Updating the certificate to support new cases such as the last signed block while keepint retro-compatibility
is difficult.
  
  The pain will mostly come from the `CertificateMessage`: we will need to keep the immutable file number in its
associated `Certificate`  in order to reconstruct the (to be removed) beacon in the message.

* **Major** The immutable file number is used alongside the epoch to deduce the 'master certificate' of an epoch. We
  need to be able to do that without it.

## Opportunities

* We can rename the legacy beacon to `CardanoDbBeacon` to explicit its role.
* We could remove the network from the legacy beacon and instead add it to the certificate metadata.
* We could remove the beacon from the certificate and instead put the signed entity type (note: we should then add the
epoch to the certificate since not all SignedEntityType will have one).

