{
  backports = {
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1ax5sqw30vdkvc7crjj2ikw9q0ayn86q2gb6yfzrkh865174vc2p";
      type = "gem";
    };
    version = "3.11.3";
  };
  daru = {
    dependencies = ["backports" "packable"];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1k72yzlzkmjg611xc9gsqx9i35g9hiyl5pxi4kczn87mb8ss0ql8";
      type = "gem";
    };
    version = "0.2.1";
  };
  #datalake = {
    #dependencies = ["daru" "json" "recursive-open-struct" "sequel"];
  #};
  json = {
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "01v6jjpvh3gnq6sgllpfqahlgxzj50ailwhj9b3cd20hi2dx0vxp";
      type = "gem";
    };
    version = "2.1.0";
  };
  packable = {
    dependencies = ["backports"];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1axhzc3867pyjwzrw8iks9dj0as3qszzh8l5xxvs8jfq68faxs50";
      type = "gem";
    };
    version = "1.3.9";
  };
  recursive-open-struct = {
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0wfcyigmf5mwrxy76p0bi4sdb4h9afs8jc73pjav5cnqszljjl3c";
      type = "gem";
    };
    version = "1.1.0";
  };
  sequel = {
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "010p4a60npppvgbyw7pq5xia8aydpgxdlhh3qjm2615kwjsw3fl8";
      type = "gem";
    };
    version = "4.49.0";
  };
}
