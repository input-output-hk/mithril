with
  signer_version as (
	select
	   content->>'$.content.party_id' as party_id,
	   content->>'$.headers.signer-node-version' as node_version,
	   content->>'$.headers.epoch' as epoch,
	   content->>'$.content.stakes' as stakes

	 from event
	 
	 order by created_at desc
  ),
  stakes_version as (
    select
      party_id,
      substr(signer_version.node_version, 0, instr(signer_version.node_version, '+')) as version,
      stakes,
      epoch

    from signer_version
    
    group by party_id, epoch
  )
select
  version,
  epoch,
  sum(stakes) as total_stakes,
  count(party_id) pool_count
from stakes_version

group by version, epoch;
