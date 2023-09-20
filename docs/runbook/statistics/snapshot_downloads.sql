select
  content->>'digest' as snapshot_digest,
  date(created_at)   as downloaded_at,
  count(*)           as downloads
from event
where
  source = 'HTTP::statistics'
  and action = 'snapshot_downloaded'
group by 1, 2
order by
  downloaded_at desc,
  downloads desc

