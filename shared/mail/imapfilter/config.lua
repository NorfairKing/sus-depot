-- Wait a little longer.
options.timeout = 120

-- Automatically set imap mailboxes to active when they are created.
options.subscribe = true

-- Create automatically
options.create = true

function main()
  gmail = IMAP {
      server = "imap.gmail.com"
    , username = "syd.kerckhove@gmail.com"
    , password = "pandemic"
    , ssl = "tls1"
  }

  kul = IMAP {
      server = "imaps.kuleuven.be"
    , username = "r0372924"
    , password = "Tp0912!!!"
    , ssl = "tls1"
  }

  eth = IMAP {
      server = "mail.ethz.ch"
    , username = "tomk"
    , password = "nilp-ab-yec-yars"
    , ssl = "tls1"
  }

  inmail = gmail['INBOX']
  rnr = gmail['rnr']
  archive = gmail['[Gmail]/All Mail']

  -- Move everything to gmail
  kulin = kul['INBOX']
  -- kulin:check_status()
  kulmail = kulin:select_all()
  kulmail:move_messages(inmail)

  ethin = eth['INBOX']
  --ethin:check_status()
  ethmail = ethin:select_all()
  ethmail:move_messages(inmail)

  inmail:check_status()

  irmails = inmail:contain_to("ir-f15@googlegroups.com")
  irmails = inmail:contain_to("ml-f15@googlegroups.com")
  irmails = inmail:contain_to("dm-f15@googlegroups.com")
  irmails = inmail:contain_to("sv-f15@googlegroups.com")
  irmails = inmail:contain_to("habitscipline@googlegroups.com")
  irmails:copy_messages(rnr)

  unseenArchived = archive:is_unseen()
  unseenArchived:mark_seen(msgs)

  -- htmlmails = inmail:match_body('.*<html>*')

  -- for k, v in pairs(inmail:fetch_message(htmlmails)) do
    -- print(k)
    -- print(v)
  -- end
end 

main()
