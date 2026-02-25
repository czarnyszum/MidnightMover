{-# LANGUAGE OverloadedStrings #-}

module TlsManager (mkTlsManager) where

import Network.HTTP.Client       (Manager, newManager)
import Network.HTTP.Client.TLS   (mkManagerSettings)
import Network.Connection        (TLSSettings (..))
import Network.TLS (defaultSupported)

-- ВАЖНО: временно отключаем проверку сертификата, чтобы убедиться,
-- что дело НЕ в EMS / протоколе. Потом включим обратно.
tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple
  { settingDisableCertificateValidation = True
  , settingDisableSession               = False
  , settingUseServerName                = True
  , settingClientSupported              = defaultSupported
  }

mkTlsManager :: IO Manager
mkTlsManager = newManager (mkManagerSettings tlsSettings Nothing)

{-

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Connection (TLSSettings (..))
import Network.TLS
import Network.TLS.Extra.Cipher (ciphersuite_default)

-- Настроим ClientParams так, чтобы отключить extendedMasterSecret
customClientParams :: ClientParams
customClientParams =
  (defaultParamsClient "" "")  -- host / SNI можно переопределить позже, нам достаточно default
  { clientSupported = (clientSupported defaultParamsClient)
                      { supportedCiphers           = ciphersuite_default
                      , supportedVersions          = [TLS13, TLS12, TLS11, TLS10]
                      , supportedExtendedMainSecret = False   -- КЛЮЧЕВОЙ МОМЕНТ
                      }
  , clientShared = (clientShared defaultParamsClient) 
  }

  -- TLSSettings, основанный на наших ClientParams
tlsSettings :: TLSSettings
tlsSettings = TLSSettings customClientParams

mkTlsManager :: IO Manager
mkTlsManager = newManager (mkManagerSettings tlsSettings Nothing)

-}
