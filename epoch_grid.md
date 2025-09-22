|Configuration parameter| Node| Epoch + 0| Epoch + 1| Epoch + 2| Epoch + 3| Epoch + 4|
| --- | --- | --- | --- | --- | --- | --- | 
|protocol parameters| aggregator (leader)| update local parameters. <br> Persist epoch setting for epoch+2 in database| advertise new protocol parameters on `/epoch-settings`| -| - | -|
|protocol parameters| aggregator (follower)| -| fetch new protocol parameters on `/epoch-settings` and does signer registration with them| -| - | -|
|protocol parameters| signer| -| fetch new protocol parameters on `/epoch-settings` <br> - signer_registration_protocol_parameters for epoch +1| -| - | -|
|entity types config| aggregator (leader) | --- | --- | --- | --- | --- | 
|entity types config| aggregator (follower) | --- | --- | --- | --- | --- | 
|entity types config| signer | --- | fetch new protocol parameters on `/epoch-settings`<br> - entity types config for epoch - 1| --- | --- | --- | 