with
  signer_version as (
    select
       content->>'$.content.party_id' as party_id,
       content->>'$.headers.signer-node-version' as node_version,
       content->>'$.headers.epoch' as epoch,
       content->>'$.content.stake' as stakes
    from event
    order by created_at desc
  ),
  stakes_version as (
    select
      party_id,
      case
        when instr(signer_version.node_version, '+') > 0
          then substr(signer_version.node_version, 0, instr(signer_version.node_version, '+'))
          else signer_version.node_version
      end as version,
      stakes,
      cast(epoch as int) as epoch
    from signer_version
    group by party_id, epoch
  ),
  summed_stakes_version as (
    select
      epoch,
      version,
      party_id,
      sum(stakes) over (partition by epoch) as total_epoch_stakes,
      sum(stakes) over (partition by epoch, version) as stakes_version
    from stakes_version
    order by epoch desc
  )
select
  epoch,
  version,
  total_epoch_stakes,
  stakes_version,
  printf('%02d %%', round((stakes_version * 100) / (total_epoch_stakes * 1.0))) as stakes_ratio,
  count(party_id) as pool_count
from summed_stakes_version
group by epoch, version
order by epoch desc, version desc
;
