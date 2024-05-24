#!/bin/bash


## Certificate file management

cert_label() {(
  set -eu -o pipefail
  md5() {
    # cat
    openssl md5 | cut -d ' ' -f 2-
  }
  for filepath in "$@"; do
    fileinfo=$(file "$(readlink -f "$filepath")")
    splitpath "$filepath"

    pem_inform=pem
    if [[ "$fileinfo" == *"data" ]]; then
      if [[ "$extension" == "der" || "$extension" == "cer" ]]; then
        pem_inform="der"
        fileinfo="$filepath: der PEM certificate"
      elif [[ "$extension" == "pfx" || "$extension" == "p12" ]]; then
        # PKCS
        text=$(openssl pkcs12 -in "$filepath" -info -nodes 2>/dev/null)
        friendly_name=$(echo "$text" | frem '/friendlyName: (.+)/')

        pemtmp=$(mktemp).pem
        echo "$text" | frem '/(-----BEGIN CERTIFICATE-----.+-----END CERTIFICATE-----)/sm' > $pemtmp
        echo "${friendly_name:-pkcs} - $(cert_label $pemtmp).p12"
        [[ -f $pemtmp ]] && rm $pemtmp
        continue
      fi
    fi

    if [[ "$fileinfo" == *"PEM certificate" ]]; then
      text="$(openssl x509 -inform $pem_inform -in "$filepath" -text -noout)"
      issue_date_a=$(echo "$text" | frem '/Not Before: .+ (\d{4}) GMT/')
      issue_date_b=$(echo "$text" | frem '/Not After ?: .+ (\d{4}) GMT/')

      issurer_cn=$(echo "$text" | frem '/Issuer: .+?CN=(.+?)(,|$)/')
      subject_cn=$(echo "$text" | frem '/Subject: .+?CN=(.+?)(,|$)/')
      issurer_o=$(echo "$text" | frem '/Issuer: .+?O=(.+?)(,|$)/')
      subject_o=$(echo "$text" | frem '/Subject: .+?O=(.+?)(,|$)/')
      issurer_ou=$(echo "$text" | frem '/Issuer: .+?OU=(.+?)(,|$)/')
      subject_ou=$(echo "$text" | frem '/Subject: .+?OU=(.+?)(,|$)/')

      cert_mod=$(openssl x509 -modulus -noout -inform $pem_inform -in "$filepath" | cut -d = -f 2-)

      echo "$(echo "$cert_mod" | md5) $issurer_o.$issurer_ou.$issurer_cn, $subject_o.$subject_ou.$subject_cn, $issue_date_a-$issue_date_b.$pem_inform"
      continue
    fi

    if [[ "$fileinfo" == *"PEM RSA private key" || "$(head -n 1 "$filepath")" == "-----BEGIN PRIVATE KEY-----" ]]; then
      key_mod=$(openssl rsa -modulus -noout -in "$filepath" | cut -d = -f 2-)
      echo $(echo "$key_mod" | md5).key
      continue
    fi

    if [[ "$fileinfo" == *"OpenSSH RSA public key" ]]; then
      key_mod=$(ssh-keygen -e -f "$filepath" -m PKCS8 | openssl rsa -inform PEM -pubin -noout -modulus | cut -d = -f 2-)
      name=$(cut -d ' ' -f 3 < "$filepath")
      echo "$(echo "$key_mod" | md5) $name.pub"
      continue
    fi

    echo Unknown file: $fileinfo > /dev/stderr
    return 1
  done
)}

cert_extract() {(
  set -eu -o pipefail
  md5() {
    # cat
    openssl md5 | cut -d ' ' -f 2-
  }
  for filepath in "$@"; do
    fileinfo=$(file "$(readlink -f "$filepath")")
    splitpath "$filepath"
    if [[ "$extension" == "pfx" || "$extension" == "p12" ]]; then
      # PKCS
      text=$(openssl pkcs12 -in "$filepath" -info -nodes 2>/dev/null)
      friendly_name=$(echo "$text" | frem '/friendlyName: (.+)/')

      pemtmp=$(mktemp).pem
      echo "$text" | frem '/(-----BEGIN ([A-Z ]+?)-----.+-----END \2-----\n)/smg' > $pemtmp
      # cat $pemtmp
      csplit -s -z -f .temp-pem- $pemtmp '/-----BEGIN/' '{*}'

      last_out=""
      for f in .temp-pem-*; do
        # echo $f
        mv "$f" "$f.pem"
        last_out="$(cert_label "$f.pem")"
        cert_label_hoist "$f.pem" || :
        rm "$f.pem"
      done
      # echo $last_out

      [[ -f $pemtmp ]] && rm $pemtmp
      continue
    fi

    if [[ "$fileinfo" == *"data" ]]; then
      if [[  "$extension" == "der" ]]; then
        pem_inform="der"
        openssl x509 -inform $pem_inform -in "$filename" -pubkey -outform pem -out "TEST_$(cert_label "$filepath" | sed 's/\....$//').$pem_inform"
      fi
    fi

    echo Unknown file: $fileinfo > /dev/stderr
    return 1
  done
)}

cert_label_hoist() {(
  set -eu -o pipefail
  for filepath in "$@"; do
    new=$(cert_label "$filepath")
    mv -i -v "$filepath" "$new" && ln -s "$new" "$filepath" || :
  done
)}

cert_sign() {(
  set -eu -o pipefail
  for filepath in "$@"; do
    splitpath "$filepath"
    if [[ "$extension" == "pem" && ! "$(grep  "KEY-----" "$filepath")" ]]; then
      mod="$(echo "$filepath" | cut -d ' ' -f 1)"
      signedpath="$filestem SIGNED.pem"
      if [[ -f "$mod.key" && ! -f "$signedpath" ]]; then
        echo "Creating signed key '$signedpath'"
        cat "$filepath" "$mod.key" > "$signedpath"
        chmod 600 "$signedpath"
      else
        echo "Missing md5'd key for cert '$filepath'"
      fi
    else
      echo "Non-cert file: $filepath"
    fi
  done
)}
