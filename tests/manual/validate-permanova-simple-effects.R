#!/usr/bin/env Rscript

# Standalone validation only; this does not implement or test a public miso API.
# Fixed protocol: 2,000 datasets (1,600 null, 400 sensitivity), 999 requested
# permutations per full/subset fit, serial RNG Mersenne-Twister/Inversion/Rejection.
# Scenarios 1-8 are null: global then conditional B effect, balanced then
# unbalanced, Euclidean then abundance. Scenarios 9-16 are sensitivity:
# additive then opposite effects, balanced then unbalanced, Euclidean then abundance.
# Seeds: data=1000000+1000*s+r; schedule=2000000+10000*s+10*r+j,
# j=0 full, j=1..6 canonical contrasts. Ant uses 3000001 full and 3000002..21.
# Support boundary: independent rows/free permutations/no covariates; ordinary
# Euclidean or fourth-root Bray-Curtis; sqrt.dist=FALSE and add=FALSE only.
# Failed planned contrasts remain in the six-row family; Holm n is always 6.
# Null decision thresholds, fixed before simulation: diagnostic >=16/200;
# hold >=22/200 for any of 56 endpoints (.05/56 one-sided exact test).
# These are validation diagnostics, not tuning criteria. No seed/repetition changes.

options(stringsAsFactors=FALSE, warn=1)
stopifnot(requireNamespace("vegan", quietly=TRUE), requireNamespace("permute", quietly=TRUE))

parse_args <- function(args=commandArgs(trailingOnly=TRUE)) {
    if (length(args) %% 2L != 0L) stop("Use --ant-data PATH --output-dir PATH")
    keys <- args[seq.int(1L, length(args), by=2L)]
    vals <- args[seq.int(2L, length(args), by=2L)]
    if (length(args) == 0L || any(!keys %in% c("--ant-data", "--output-dir")) ||
        anyDuplicated(keys) || !all(c("--ant-data", "--output-dir") %in% keys))
        stop("Use exactly one --ant-data PATH and --output-dir PATH")
    setNames(vals, sub("^--", "", keys))
}

check_results <- character()
assert <- function(ok, label) {
    if (!isTRUE(ok)) stop("CHECK FAILED: ", label, call.=FALSE)
    check_results <<- c(check_results, label)
    message("PASS: ", label)
}
close_enough <- function(x, y, tol=1e-10) is.finite(x) && is.finite(y) &&
    abs(x-y) <= tol * max(1, abs(y))
set_rng <- function(seed) {
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    set.seed(as.integer(seed))
}

planned_pairs <- function(a_levels, b_levels) {
    unlist(lapply(b_levels, function(b) {
        pairs <- utils::combn(a_levels, 2L, simplify=FALSE)
        lapply(pairs, function(p) list(a1=p[[1L]], a2=p[[2L]], b=b))
    }), recursive=FALSE)
}
contrast_id <- function(p) paste(p$b, p$a1, p$a2, sep="::")

# One shared complete-row mask preserves immutable IDs. Only abundance data
# drop all-zero rows; signed Euclidean observations may sum to zero.
preprocess <- function(x, metadata, id=rownames(x), abundance=FALSE) {
    if (is.null(id)) id <- as.character(seq_len(nrow(x)))
    if (nrow(x) != nrow(metadata) || length(id) != nrow(x)) stop("row alignment mismatch")
    keep <- complete.cases(x) & complete.cases(metadata)
    if (abundance) keep <- keep & rowSums(x, na.rm=TRUE) != 0
    list(x=x[keep,,drop=FALSE], metadata=metadata[keep,,drop=FALSE], ids=id[keep], mask=keep)
}

validate_options <- function(permutation="free", covariates=NULL, sqrt.dist=FALSE, add=FALSE,
                             distance="euclidean") {
    if (!identical(permutation, "free")) stop("unsupported permutation restriction")
    if (!is.null(covariates) && length(covariates)) stop("unsupported covariates")
    if (isTRUE(sqrt.dist)) stop("unsupported sqrt.dist=TRUE")
    if (!identical(add, FALSE)) stop("unsupported distance correction")
    if (!distance %in% c("euclidean", "bray-fourth-root")) stop("unsupported distance")
    invisible(TRUE)
}

# Actual helper used by the simulations and deterministic tests. Planned rows
# are never dropped; unsupported/degenerate rows keep a specific failure status.
fit_planned <- function(dist, metadata, ids, A="A", B="B", permutations,
                        family_n=NULL, options=list()) {
    validate_options(options$permutation %||% "free", options$covariates,
        options$sqrt.dist %||% FALSE, options$add %||% FALSE,
        options$distance %||% "euclidean")
    if (is.null(rownames(as.matrix(dist))) || !identical(rownames(as.matrix(dist)), ids) ||
        !identical(rownames(metadata), ids)) stop("misaligned immutable row IDs")
    a_levels <- if(is.factor(metadata[[A]])) levels(metadata[[A]]) else unique(as.character(metadata[[A]]))
    b_levels <- if(is.factor(metadata[[B]])) levels(metadata[[B]]) else unique(as.character(metadata[[B]]))
    aa <- droplevels(factor(metadata[[A]])); bb <- droplevels(factor(metadata[[B]]))
    pairs <- planned_pairs(a_levels, b_levels)
    m <- length(pairs); if (is.null(family_n)) family_n <- m
    if (family_n != m) stop("planned family size mismatch")
    out <- lapply(seq_along(pairs), function(i) {
        p <- pairs[[i]]; keep <- bb == p$b & aa %in% c(p$a1, p$a2)
        sid <- ids[keep]; ng <- table(droplevels(aa[keep]))
        row <- data.frame(contrast=contrast_id(p), a1=p$a1, a2=p$a2, b=p$b,
            n1=unname(ng[p$a1] %||% 0L), n2=unname(ng[p$a2] %||% 0L),
            status="failed", reason="", F=NA_real_, p=NA_real_, padj=NA_real_,
            stringsAsFactors=FALSE)
        if (length(sid) < 4L || length(ng) != 2L || any(ng < 2L)) {
            row$reason <- "fewer than two observations per group"; return(row)
        }
        dsub <- as.dist(as.matrix(dist)[sid,sid,drop=FALSE])
        g <- droplevels(aa[keep]); mm <- stats::model.matrix(~g)
        if (length(sid)-qr(mm)$rank <= 0L) { row$reason <- "nonpositive residual df"; return(row) }
        ds <- as.numeric(dsub)
        if (!length(ds) || any(!is.finite(ds)) || all(ds == 0)) {
            row$reason <- "zero-total or nonfinite distance"; return(row)
        }
        fit <- tryCatch(vegan::adonis2(dsub ~ g, data=data.frame(g=g),
            permutations=permutations[[i]], by="terms", sqrt.dist=FALSE, add=FALSE),
            error=function(e)e)
        if (inherits(fit,"error")) { row$reason <- paste("adonis2:", conditionMessage(fit)); return(row) }
        tab <- as.data.frame(fit); k <- match("g", rownames(tab))
        if (is.na(k) || !is.finite(tab$F[k]) || !is.finite(tab[["Pr(>F)"]][k]) ||
            !is.finite(tab$SumOfSqs[match("Residual",rownames(tab))]) ||
            tab$SumOfSqs[match("Residual",rownames(tab))] <= 0) {
            row$reason <- "degenerate/nonfinite fitted statistic or residual SS"; return(row)
        }
        row$status <- "ok"; row$reason <- ""; row$F <- tab$F[k]; row$p <- tab[["Pr(>F)"]][k]
        row
    })
    out <- do.call(rbind, out)
    ok <- out$status == "ok"
    if (any(ok)) out$padj[ok] <- stats::p.adjust(out$p[ok], method="holm", n=family_n)
    out
}
`%||%` <- function(x,y) if (is.null(x)) y else x

schedule <- function(n, seed, nperm=999L) {
    set_rng(seed)
    permute::shuffleSet(n, nset=as.integer(nperm), control=permute::how(nperm=as.integer(nperm)))
}
fit_full <- function(dist, metadata, ids, P, A="A", B="B") {
    if (!identical(rownames(metadata), ids) || !identical(rownames(as.matrix(dist)), ids))
        stop("misaligned full-model row IDs")
    mm <- stats::model.matrix(stats::as.formula(paste("~", A, "*", B)), data=metadata)
    if (nrow(metadata) - qr(mm)$rank <= 0L) stop("full model is saturated (no residual degrees of freedom)")
    d <- dist
    vegan::adonis2(stats::as.formula(paste("d ~", A, "*", B)), data=metadata, permutations=P, by="margin",
                   sqrt.dist=FALSE, add=FALSE)
}

manual_f <- function(y,k) {
    x <- y[seq_len(k)]; z <- y[-seq_len(k)]
    sp2 <- (sum((x-mean(x))^2)+sum((z-mean(z))^2))/(length(y)-2L)
    (mean(x)-mean(z))^2/(sp2*(1/length(x)+1/length(z)))
}
manual_holm <- function(p,m) {
    ok<-!is.na(p); z<-ifelse(ok,p,1); ord<-order(z); adjusted<-numeric(length(z))
    adjusted[ord]<-pmin(1,cummax((m-seq_along(ord)+1L)*z[ord])); adjusted[!ok]<-NA_real_; adjusted
}
exact_oracle <- function(k) {
    y <- c(1,2,3,5,6,8,9,12); alloc <- utils::combn(8L,k)
    perms <- t(apply(alloc,2L,function(s)c(s,setdiff(seq_len(8L),s))))
    stopifnot(all(apply(perms,1L,function(z)identical(sort(z),seq_len(8L)))))
    observed <- manual_f(y,k)
    vals <- apply(alloc,2L,function(s)manual_f(y[c(s,setdiff(seq_len(8L),s))],k))
    p <- sum(vals >= observed-sqrt(.Machine$double.eps))/length(vals)
    # Keep one representative observed allocation out of vegan's supplied rows.
    take <- which(apply(perms,1L,function(z)identical(z,seq_len(8L))))[1L]
    pv <- perms[-take,,drop=FALSE]
    dv <- stats::dist(y)
    fit <- vegan::adonis2(dv ~ g, data=data.frame(g=factor(rep(c("a","b"),c(k,8-k)))),
                           permutations=pv, by="terms")
    list(F=observed,F_vegan=fit$F[1L],p=p,p_vegan=fit[["Pr(>F)"]][1L], nperm=nrow(pv), all=perms)
}

pair_ss <- function(d, group) {
    d <- as.matrix(d); n <- nrow(d)
    total <- sum(d[upper.tri(d)]^2)/n
    within <- sum(vapply(split(seq_len(n),group),function(ii) {
        if(length(ii)<2L) 0 else sum(d[ii,ii,drop=FALSE][upper.tri(d[ii,ii,drop=FALSE])]^2)/length(ii)
    },numeric(1)))
    c(total=total,within=within,between=total-within,
      F=(total-within)/(within/(n-2L)),df.residual=n-2L,r2=(total-within)/total)
}

run_deterministic <- function(outdir) {
    check <- character()
    # Manual univariate and exact enumeration oracles.
    for (k in c(4L,3L)) {
        z <- exact_oracle(k); expected <- if(k==4L) 15.7090909090909 else 12.65625
        ep <- if(k==4L) 2/70 else 2/56
        assert(close_enough(manual_f(c(1,2,3,5,6,8,9,12),k),expected),paste("manual F",k))
        assert(close_enough(z$F,expected) && close_enough(z$F_vegan,manual_f(c(1,2,3,5,6,8,9,12),k)) &&
               abs(z$p-ep)<1e-12 && abs(z$p_vegan-ep)<1e-12,
               paste("manual and vegan F, exact enumeration and correction",k))
        y<-c(1,2,3,5,6,8,9,12); tt<-stats::t.test(y[seq_len(k)],y[-seq_len(k)],var.equal=TRUE)$statistic^2
        assert(close_enough(tt,expected),paste("equal-variance t oracle",k))
    }
    raw <- c(.01,.03,NA,.20,NA,NA); ok<-!is.na(raw)
    adj<-stats::p.adjust(ifelse(ok,raw,1),method="holm",n=6L); adj[!ok]<-NA_real_
    independent<-manual_holm(raw,6L)
    assert(identical(unname(round(adj[ok],8)),c(.06,.15,.80)) && identical(unname(adj),unname(independent)),"Holm complete-family independent reference")
    assert(length(planned_pairs(LETTERS[1:3],c("B1","B2")))==6L,"six planned comparisons")
    # Exercise unsupported covariates and restrictions through the actual fitter.
    test_x<-matrix(seq_len(24),ncol=2); rownames(test_x)<-paste0("o",1:12)
    test_md<-data.frame(A=factor(rep(LETTERS[1:3],each=4)),B=factor(rep(c("b1","b2"),6)))
    rownames(test_md)<-rownames(test_x); test_p<-list(matrix(rep(1:12,9),nrow=9,byrow=TRUE))
    bad_options<-list(
        list(covariates=data.frame(varying=seq_len(12))),
        list(covariates=data.frame(constant=rep(1,12))),
        list(covariates=data.frame(aliased=as.integer(test_md$A))),
        list(sqrt.dist=TRUE),list(add="lingoes"),list(distance="other"),
        list(permutation=permute::how(blocks=factor(rep(1:6,each=2)))),
        list(permutation=permute::how(blocks=factor(seq_len(12)))),
        list(permutation=permute::how(within=permute::Within(type="series"))))
    for (v in bad_options)
        assert(inherits(try(fit_planned(stats::dist(test_x),test_md,rownames(test_md),permutations=test_p,options=v),silent=TRUE),"try-error"),
               paste("fit_planned rejects unsupported options",paste(names(v),collapse=",")))
    # One shared complete-case mask; all-zero filtering is abundance-only.
    x<-matrix(c(1,2,0, 2,1,0, NA,1,0, 0,0,0, 4,5,0, 5,4,0, -1,1,0),ncol=3,byrow=TRUE)
    md<-data.frame(A=letters[c(1,1,1,2,2,2,2)],B="B1"); md$A[6]<-NA
    rownames(md)<-paste0("id",1:7); rownames(x)<-rownames(md)
    pp<-preprocess(x,md); assert(identical(pp$ids,c("id1","id2","id4","id5","id7")),"complete response/metadata mask preserves signed zero-sum rows and IDs")
    pa<-preprocess(x,md,abundance=TRUE); assert(identical(pa$ids,c("id1","id2","id5")),"abundance preprocessing removes all-zero row only")
    md_bad<-md[1:2,,drop=FALSE]; rownames(md_bad)<-rev(rownames(md_bad))
    xx_bad<-x[1:2,,drop=FALSE]
    assert(inherits(try(fit_planned(stats::dist(xx_bad),md_bad,c("id1","id2"),permutations=list(matrix(1:2,nrow=1))),silent=TRUE),"try-error"),"detect actual metadata/distance ID mismatch")
    # Delete only A3/B2 from a 3x2 design: six planned rows, two failed.
    set_rng(431); mm0<-expand.grid(A=factor(c("A1","A2","A3")),B=factor(c("B1","B2")),rep=1:2)
    mm0<-mm0[!(mm0$A=="A3" & mm0$B=="B2"),]; xx<-matrix(rnorm(nrow(mm0)*3),ncol=3)
    rownames(xx)<-paste0("r",seq_len(nrow(xx))); rownames(mm0)<-rownames(xx)
    mm<-data.frame(A=factor(mm0$A,levels=c("A1","A2","A3")),B=factor(mm0$B,levels=c("B1","B2"))); rownames(mm)<-rownames(xx)
    pairs0<-planned_pairs(levels(mm$A),levels(mm$B))
    ps<-lapply(seq_along(pairs0),function(i){p<-pairs0[[i]];n<-sum(mm$B==p$b & mm$A %in% c(p$a1,p$a2));schedule(n,430L+i,9L)})
    fr<-fit_planned(stats::dist(xx),mm,rownames(mm),permutations=ps,family_n=6L)
    assert(nrow(fr)==6L && sum(fr$status!="ok")==2L && sum(fr$status=="ok")==4L,"two unavailable comparisons retained in six-row family")
    assert(identical(fr$contrast, vapply(pairs0,contrast_id,character(1))),"canonical planned IDs")
    assert(max(abs(fr$padj-manual_holm(fr$p,6L)),na.rm=TRUE)<1e-12 && all(is.na(fr$padj[fr$status!="ok"])),"planned Holm uses m=6 including failures")
    small_case <- function(counts, values=seq_len(sum(counts))) {
        xx<-matrix(rep(values,length.out=sum(counts)*2L),ncol=2L); rownames(xx)<-paste0("c",seq_len(nrow(xx)))
        md<-data.frame(A=factor(rep(c("A1","A2"),counts),levels=c("A1","A2")),B=factor(rep("B1",sum(counts)),levels="B1")); rownames(md)<-rownames(xx)
        pp<-list(matrix(rep(seq_len(nrow(xx)),9L),nrow=9L,byrow=TRUE))
        fit_planned(stats::dist(xx),md,rownames(md),permutations=pp)
    }
    assert(small_case(c(1L,1L))$reason[[1L]]=="fewer than two observations per group","reject 1+1 subset")
    assert(small_case(c(1L,3L))$reason[[1L]]=="fewer than two observations per group","reject 1+3 subset")
    assert(grepl("zero-total",small_case(c(2L,2L),rep(1,4))$reason[[1L]],fixed=TRUE),"reject identical/zero-total distances")
    assert(inherits(try(validate_options(permutation="singleton-block"),silent=TRUE),"try-error") &&
        inherits(try(validate_options(permutation="ineffective-block"),silent=TRUE),"try-error"),"reject ineffective/singleton block restrictions")
    assert(inherits(try(validate_options(permutation="series"),silent=TRUE),"try-error"),"reject series schedule")
    # Deterministic 3x2 four-per-cell fixture, direct SS oracles for Euclidean/Bray.
    design<-expand.grid(A=factor(c("A1","A2","A3")),B=factor(c("B1","B2")),rep=1:4)
    set_rng(904); resp<-matrix(runif(nrow(design)*5),ncol=5); rownames(resp)<-paste0("d",seq_len(nrow(resp)))
    rownames(design)<-rownames(resp)
    for (metric in c("euclidean","bray-fourth-root")) {
        z<-if(metric=="euclidean") resp else vegan::vegdist(resp^(1/4),method="bray")
        if(metric=="euclidean") z<-stats::dist(resp)
        dm<-as.matrix(z)
        pairs_ss<-planned_pairs(levels(design$A),levels(design$B))
        ps_ss<-lapply(seq_along(pairs_ss),function(i){p<-pairs_ss[[i]]; schedule(8L,990L+i,99L)})
        planned_ss<-fit_planned(as.dist(dm),design,rownames(design),permutations=ps_ss,family_n=6L)
        for (b in levels(design$B)) for (pair in utils::combn(levels(design$A),2,simplify=FALSE)) {
            use<-design$B==b & design$A %in% pair; ids<-rownames(design)[use]
            ref<-pair_ss(dm[ids,ids,drop=FALSE],droplevels(design$A[use]))
            recomputed<-if(metric=="euclidean") stats::dist(resp[use,,drop=FALSE]) else vegan::vegdist(resp[use,,drop=FALSE]^(1/4),method="bray")
            assert(max(abs(as.numeric(recomputed)-as.numeric(stats::as.dist(dm[ids,ids,drop=FALSE]))))<1e-12,paste("subset distance recomputation",metric,b,pair[1],pair[2]))
            fit<-vegan::adonis2(as.dist(dm[ids,ids,drop=FALSE]) ~ g,
                data=data.frame(g=droplevels(design$A[use])),permutations=0,by="terms")
            tab<-as.data.frame(fit); r<-match("g",rownames(tab)); rr<-match("Residual",rownames(tab))
            assert(close_enough(ref["F"],tab$F[r]) && close_enough(ref["between"],tab$SumOfSqs[r]) &&
                close_enough(ref["within"],tab$SumOfSqs[rr]) && ref["df.residual"]==tab$Df[rr] &&
                close_enough(ref["r2"],tab$R2[r]),paste("distance arithmetic",metric,b,pair[1],pair[2]))
            pi<-match(contrast_id(list(a1=pair[1],a2=pair[2],b=b)),planned_ss$contrast)
            assert(planned_ss$status[pi]=="ok" && close_enough(ref["F"],planned_ss$F[pi]),
                   paste("fit_planned subset F arithmetic",metric,b,pair[1],pair[2]))
        }
    }
    # Exact schedule repetition, factor-level invariance, mapped row-order invariance.
    set_rng(88); dat<-matrix(rnorm(24),nrow=12); rownames(dat)<-paste0("x",1:12)
    meta<-data.frame(A=factor(rep(LETTERS[1:3],each=4)),B=factor(rep(c("b1","b2"),6)))
    rownames(meta)<-rownames(dat); dis<-stats::dist(dat)
    P<-schedule(12,998,99); P2<-schedule(12,998,99)
    assert(identical(P,P2),"identical schedule regeneration")
    s1<-fit_full(dis,meta,rownames(meta),P)
    s1repeat<-fit_full(dis,meta,rownames(meta),P)
    assert(identical(s1,s1repeat),"repeated fit results with identical schedule")
    revmeta<-meta; revmeta$A<-factor(revmeta$A,levels=rev(levels(meta$A))); revmeta$B<-factor(revmeta$B,levels=rev(levels(meta$B)))
    s2<-fit_full(dis,revmeta,rownames(revmeta),P)
    assert(close_enough(s1$F[1],s2$F[1]) && close_enough(s1[["Pr(>F)"]][1],s2[["Pr(>F)"]][1]),"factor-level reversal invariant")
    q<-c(4,1,8,2,12,5,3,9,6,10,7,11); mapped<-t(apply(P,1,function(p)match(p[q],q)))
    sd2<-stats::dist(dat[q,]); md2<-meta[q,,drop=FALSE]
    s3<-fit_full(sd2,md2,rownames(md2),mapped)
    assert(close_enough(s1$F[1],s3$F[1]) && close_enough(s1[["Pr(>F)"]][1],s3[["Pr(>F)"]][1]),"mapped observation reordering invariant")
    assert("A:B" %in% rownames(s1) && !all(c("A","B") %in% rownames(s1)),"marginal interaction model reports interaction, not main effects")
    # fit_planned invariance uses matching unordered comparison schedules.
    planned_schedules<-function(md,seed){
        pp<-planned_pairs(levels(md$A),levels(md$B))
        lapply(seq_along(pp),function(i){p<-pp[[i]]; n<-sum(md$B==p$b & md$A %in% c(p$a1,p$a2)); schedule(n,seed+i,99L)})
    }
    ps_planned<-planned_schedules(meta,5000)
    fam1<-fit_planned(dis,meta,rownames(meta),permutations=ps_planned,family_n=6L)
    fam_repeat<-fit_planned(dis,meta,rownames(meta),permutations=ps_planned,family_n=6L)
    assert(identical(fam1,fam_repeat),"fit_planned repeated results with identical matrices")
    key<-function(z) paste(z$b,paste(sort(c(z$a1,z$a2)),collapse="::"),sep="::")
    rev_pairs<-planned_pairs(levels(revmeta$A),levels(revmeta$B))
    base_keys<-vapply(planned_pairs(levels(meta$A),levels(meta$B)),key,character(1))
    rev_keys<-vapply(rev_pairs,key,character(1))
    ps_rev<-lapply(rev_keys,function(k)ps_planned[[match(k,base_keys)]])
    fam_rev<-fit_planned(dis,revmeta,rownames(revmeta),permutations=ps_rev,family_n=6L)
    keys1<-vapply(planned_pairs(levels(meta$A),levels(meta$B)),key,character(1))
    keys2<-vapply(planned_pairs(levels(revmeta$A),levels(revmeta$B)),key,character(1))
    names1<-setNames(seq_along(keys1),keys1); names2<-setNames(seq_along(keys2),keys2)
    assert(all(keys1 %in% keys2) && all(vapply(keys1,function(k){i<-names1[[k]];j<-names2[[k]];
        fam1$status[i]==fam_rev$status[j] && close_enough(fam1$F[i],fam_rev$F[j]) &&
        close_enough(fam1$p[i],fam_rev$p[j]) && close_enough(fam1$padj[i],fam_rev$padj[j])},logical(1))),
        "fit_planned factor reversal invariance by unordered IDs")
    fam_reorder_schedules<-function(md_new,q,seed){
        old_pairs<-planned_pairs(levels(meta$A),levels(meta$B)); new_pairs<-planned_pairs(levels(md_new$A),levels(md_new$B))
        old_keys<-vapply(old_pairs,key,character(1)); new_keys<-vapply(new_pairs,key,character(1)); old_schedules<-planned_schedules(meta,seed)
        lapply(seq_along(new_pairs),function(j){k<-new_keys[j];i<-match(k,old_keys);p<-new_pairs[[j]]
            old_ids<-rownames(meta)[meta$B==p$b & meta$A %in% c(p$a1,p$a2)]
            new_ids<-rownames(md_new)[md_new$B==p$b & md_new$A %in% c(p$a1,p$a2)]
            qsub<-match(new_ids,old_ids)
            t(apply(old_schedules[[i]],1,function(row)match(row[qsub],qsub)))})
    }
    fam_mapped<-fit_planned(stats::dist(dat[q,]),md2,rownames(md2),permutations=fam_reorder_schedules(md2,q,5000),family_n=6L)
    mapped_keys<-vapply(planned_pairs(levels(md2$A),levels(md2$B)),key,character(1)); mapped_idx<-setNames(seq_along(mapped_keys),mapped_keys)
    assert(all(vapply(keys1,function(k){i<-names1[[k]];j<-mapped_idx[[k]];
        fam1$status[i]==fam_mapped$status[j] && close_enough(fam1$F[i],fam_mapped$F[j]) &&
        close_enough(fam1$p[i],fam_mapped$p[j]) && close_enough(fam1$padj[i],fam_mapped$padj[j])},logical(1))),
        "fit_planned mapped observation reordering invariant")
    saturated_md<-data.frame(A=factor(rep(LETTERS[1:3],each=2)),B=factor(rep(c("b1","b2"),3)))
    saturated_ids<-paste0("sat",1:6); rownames(saturated_md)<-saturated_ids
    saturated_d<-stats::dist(matrix(seq_len(12),ncol=2,dimnames=list(saturated_ids,NULL)))
    assert(grepl("saturated",tryCatch(fit_full(saturated_d,saturated_md,saturated_ids,matrix(1:6,nrow=1)),error=function(e)conditionMessage(e)),fixed=TRUE),
           "full-model saturated zero-residual case explicitly rejected")
    # Generator contract: only feature 1 receives the Euclidean effect.
    generated<-simulate_data("additive","balanced","euclidean",1L,9L)
    set_rng(1000000L+1000L*9L+1L); noise<-2+matrix(runif(48L*3L,-1,1),ncol=3L)
    eff<-c(-.6,0,.6)[as.integer(generated$meta$A)]+ifelse(generated$meta$B=="B2",.5,0)
    assert(identical(generated$Y[,2:3],noise[,2:3]) && all(abs((generated$Y[,1L]-noise[,1L])-eff)<1e-14),
           "Euclidean generator applies effect only to feature 1")
    write.csv(data.frame(check=check_results,result="passed"),file.path(outdir,"deterministic-checks.csv"),row.names=FALSE)
    invisible(TRUE)
}

simulate_data <- function(scenario, size, metric, rep, sid) {
    cells <- if(size=="balanced") rep(8L,6L) else c(4L,7L,10L,9L,6L,12L)
    # Expand in A1-A3 within B1, then A1-A3 within B2.
    Alev<-paste0("A",1:3); Blev<-c("B1","B2")
    A<-factor(rep(rep(Alev,times=2L),cells),levels=Alev)
    B<-factor(rep(rep(Blev,each=3L),cells),levels=Blev)
    set_rng(1000000L+1000L*sid+rep)
    if(metric=="euclidean") {
        effect<-switch(scenario,global=rep(0,3),conditional=c(0,0,0),additive=c(-.6,0,.6),opposite=c(-.6,0,.6))
        if(scenario=="conditional") effect<-ifelse(B=="B2",.5,0)
        if(scenario=="additive") effect<-effect[as.integer(A)] + ifelse(B=="B2",.5,0)
        if(scenario=="opposite") effect<-effect[as.integer(A)] * ifelse(B=="B1",-1,1)
        Y<-2+matrix(runif(length(A)*3L,-1,1),ncol=3)
        Y[,1L]<-Y[,1L]+effect
    } else {
        a<-c(-.7,0,.7); effect<-switch(scenario,global=rep(0,length(A)),conditional=rep(0,length(A)),additive=a[as.integer(A)]+ifelse(B=="B2",.5,0),opposite=a[as.integer(A)]*ifelse(B=="B1",-1,1))
        if(scenario=="conditional") effect<-ifelse(B=="B2",.5,0)
        base<-c(5,10,15,20,25,30); loading<-c(1,-1,.5,0,0,0)
        lambda<-matrix(rep(base,each=length(A)),nrow=length(A))*exp(effect%o%loading)
        Y<-matrix(rpois(length(A)*6L,lambda),nrow=length(A))
        if(any(rowSums(Y)==0)) stop("supported-simulation failure: unexpected all-zero abundance row")
        Y<-Y^(1/4)
    }
    list(Y=Y,meta=data.frame(A=A,B=B),ids=paste0("s",sid,"r",rep,"i",seq_along(A)))
}

scenario_table <- function() {
    z<-expand.grid(metric=c("euclidean","abundance"),size=c("balanced","unbalanced"),
                   scenario=c("global","conditional"),stringsAsFactors=FALSE)
    z<-z[order(match(z$scenario,c("global","conditional")),match(z$size,c("balanced","unbalanced")),match(z$metric,c("euclidean","abundance"))),]
    n<-nrow(z); z$kind<-"null"; z$replicates<-200L; z$id<-seq_len(n)
    s<-expand.grid(metric=c("euclidean","abundance"),size=c("balanced","unbalanced"),
                   scenario=c("additive","opposite"),stringsAsFactors=FALSE)
    s<-s[order(match(s$scenario,c("additive","opposite")),match(s$size,c("balanced","unbalanced")),match(s$metric,c("euclidean","abundance"))),]
    s$kind<-"sensitivity"; s$replicates<-50L; s$id<-n+seq_len(nrow(s))
    rbind(z,s)
}

run_simulations <- function(outdir) {
    configs<-scenario_table(); write.csv(configs,file.path(outdir,"scenario-schedule.csv"),row.names=FALSE)
    results<-list(); schedules<-list(); nr<-0L
    for (ci in seq_len(nrow(configs))) {
        cfg<-configs[ci,]
        message(sprintf("Scenario %d/16: %s %s %s (%d replicates)",cfg$id,cfg$scenario,cfg$size,cfg$metric,cfg$replicates))
        for (r in seq_len(cfg$replicates)) {
            dat<-simulate_data(cfg$scenario,cfg$size,cfg$metric,r,cfg$id)
            transformed<-dat$Y
            dist<-if(cfg$metric=="euclidean") stats::dist(transformed) else vegan::vegdist(transformed,method="bray")
            rownames(dat$meta)<-dat$ids
            # Store row IDs on the distance matrix via a labelled dist reconstruction.
            D<-as.matrix(dist); dimnames(D)<-list(dat$ids,dat$ids); dist<-stats::as.dist(D)
            Pfull<-schedule(length(dat$ids),2000000L+10000L*cfg$id+10L*r,999L)
            pairs<-planned_pairs(levels(dat$meta$A),levels(dat$meta$B))
            Ps<-lapply(seq_along(pairs),function(j) {
                p<-pairs[[j]]; n<-sum(dat$meta$B==p$b & dat$meta$A %in% c(p$a1,p$a2))
                schedule(n,2000000L+10000L*cfg$id+10L*r+j,999L)
            })
            if(r==1L) {
                key<-paste0("scenario",cfg$id)
                schedules[[key]]<-list(full=Pfull,contrasts=Ps,ids=dat$ids,
                    permutation_seeds=c(2000000L+10000L*cfg$id+10L*r,2000000L+10000L*cfg$id+10L*r+seq_along(Ps)),
                    requested=c(999L,rep(999L,length(Ps))),actual=c(nrow(Pfull),vapply(Ps,nrow,integer(1))))
                assert(identical(Pfull,schedule(length(dat$ids),2000000L+10000L*cfg$id+10L*r,999L)),paste("representative full schedule",key))
                assert(all(vapply(seq_along(Ps),function(j) {
                    p<-pairs[[j]]; nsub<-sum(dat$meta$B==p$b & dat$meta$A %in% c(p$a1,p$a2))
                    identical(Ps[[j]],schedule(nsub,2000000L+10000L*cfg$id+10L*r+j,999L))
                },logical(1))),paste("representative subset schedules",key))
            }
            full<-fit_full(dist,dat$meta,dat$ids,Pfull)
            fam<-fit_planned(dist,dat$meta,dat$ids,"A","B",Ps,family_n=6L)
            if(r==1L) {
                full_repeat<-fit_full(dist,dat$meta,dat$ids,Pfull)
                fam_repeat<-fit_planned(dist,dat$meta,dat$ids,"A","B",Ps,family_n=6L)
                assert(identical(full,full_repeat) && identical(fam,fam_repeat),paste("representative repeated fits",cfg$id))
            }
            if(any(fam$status!="ok")) stop("supported simulation comparison failed: ",paste(fam$contrast[fam$status!="ok"],fam$reason[fam$status!="ok"],collapse="; "))
            inter<-match("A:B",rownames(full)); if(is.na(inter)) stop("full marginal interaction row missing")
            nr<-nr+1L
            results[[nr]]<-data.frame(scenario_id=cfg$id,scenario=cfg$scenario,kind=cfg$kind,size=cfg$size,metric=cfg$metric,replicate=r,
                data_seed=1000000L+1000L*cfg$id+r,full_seed=2000000L+10000L*cfg$id+10L*r,
                full_f=full$F[inter],full_r2=full$R2[inter],full_p=full[["Pr(>F)"]][inter],
                full_perm_requested=999L,full_perm_actual=nrow(Pfull),
                interaction_p=full[["Pr(>F)"]][inter],contrast_seed=2000000L+10000L*cfg$id+10L*r+seq_along(Ps),contrast_perm_requested=999L,
                contrast_perm_actual=vapply(Ps,nrow,integer(1)),status_count=sum(fam$status=="ok"),failure_count=sum(fam$status!="ok"),
                contrast=fam$contrast,conditionalF=fam$F,status=fam$status,reason=fam$reason,raw_p=fam$p,holm_p=fam$padj)
        }
    }
    all<-do.call(rbind,results); write.csv(all,file.path(outdir,"simulation-results.csv"),row.names=FALSE)
    saveRDS(schedules,file.path(outdir,"representative-schedules.rds"))
    # Endpoint counts and exact uncertainty; no pooled contrast denominator.
    summaries<-list(); k<-0L
    for (sid in 1:8) {
        d<-all[all$scenario_id==sid,]; for (endpoint in c(sort(unique(d$contrast)),"holm-family")) {
            v<-if(endpoint=="holm-family") vapply(split(d$holm_p,d$replicate),function(p)any(p<=.05),logical(1)) else d$raw_p[d$contrast==endpoint]<=.05
            count<-sum(v); bt<-stats::binom.test(count,200,p=.05,alternative="greater")
            ci<-stats::binom.test(count,200)$conf.int
            k<-k+1L; summaries[[k]]<-data.frame(scenario_id=sid,endpoint=endpoint,rejections=count,n=200,
                rate=count/200,ci_low=ci[1],ci_high=ci[2],one_sided_p=bt$p.value,
                diagnostic_warning=bt$p.value<=.05,acceptance_hold=bt$p.value<=.05/56)
        }
    }
    nullsum<-do.call(rbind,summaries); write.csv(nullsum,file.path(outdir,"null-inflation-summary.csv"),row.names=FALSE)
    sens<-all[all$kind=="sensitivity",]; sens$reject_raw<-sens$raw_p<=.05; sens$reject_holm<-sens$holm_p<=.05
    sensitivity<-aggregate(cbind(reject_raw,reject_holm)~scenario_id+scenario+size+metric+contrast,sens,mean)
    inter<-aggregate(interaction_p~scenario_id+scenario+size+metric,sens,function(p)mean(p<=.05))
    write.csv(sensitivity,file.path(outdir,"sensitivity-summary.csv"),row.names=FALSE); write.csv(inter,file.path(outdir,"sensitivity-interaction-summary.csv"),row.names=FALSE)
    list(configs=configs,results=all,null=nullsum,sensitivity=sensitivity,interaction=inter)
}

run_ant <- function(path,outdir) {
    raw<-read.csv(path,check.names=FALSE,stringsAsFactors=FALSE)
    if(nrow(raw)!=76L || ncol(raw)!=103L) stop("Ant input dimensions differ from preregistered 76x103")
    X<-as.matrix(raw[,4:103,drop=FALSE]); storage.mode(X)<-"numeric"
    labels<-names(raw)[4:103]; dup<-duplicated(labels)
    mapping<-data.frame(position=4:103,column_label=labels,occurrence=ave(seq_along(labels),labels,FUN=seq_along))
    meta<-data.frame(Community=factor(raw$Community),Sample=factor(raw$Sample),row.names=paste0("ant",seq_len(nrow(raw))))
    rownames(X)<-rownames(meta); pp<-preprocess(X,meta,rownames(meta),abundance=TRUE); X<-pp$x; meta<-pp$metadata; ids<-pp$ids
    if(any(rowSums(X)==0)) stop("Ant preprocessing unexpectedly retained zero abundance row")
    d<-vegan::vegdist(X^(1/4),method="bray"); D<-as.matrix(d);dimnames(D)<-list(ids,ids);d<-stats::as.dist(D)
    P<-schedule(length(ids),3000001,999L); full<-fit_full(d,meta,ids,P,"Community","Sample")
    pairs<-planned_pairs(levels(meta$Community),levels(meta$Sample))
    Ps<-lapply(seq_along(pairs),function(i){p<-pairs[[i]];n<-sum(meta$Sample==p$b & meta$Community %in% c(p$a1,p$a2));schedule(n,3000001L+i,999L)})
    fam<-fit_planned(d,meta,ids,"Community","Sample",Ps,family_n=20L,
        options=list(distance="bray-fourth-root"))
    write.csv(mapping,file.path(outdir,"ant-column-mapping.csv"),row.names=FALSE)
    write.csv(fam,file.path(outdir,"ant-comparisons.csv"),row.names=FALSE)
    write.csv(as.data.frame(full),file.path(outdir,"ant-full-model.csv"),row.names=TRUE)
    write.csv(data.frame(id=ids,Community=meta$Community,Sample=meta$Sample),file.path(outdir,"ant-retained-rows.csv"),row.names=FALSE)
    write.csv(as.data.frame.matrix(table(meta$Community,meta$Sample)),file.path(outdir,"ant-cell-sizes.csv"),row.names=TRUE)
    write.csv(data.frame(endpoint=c("full",vapply(seq_along(pairs),function(i)contrast_id(pairs[[i]]),character(1))),
        seed=c(3000001L,3000001L+seq_along(Ps)),requested=999L,actual=c(nrow(P),vapply(Ps,nrow,integer(1)))),file.path(outdir,"ant-schedule-summary.csv"),row.names=FALSE)
    saveRDS(list(full=P,comparisons=Ps,ids=ids),file.path(outdir,"ant-schedules.rds"))
    list(rows=length(ids),cells=table(meta$Community,meta$Sample),duplicate_labels=labels[dup],failures=fam[fam$status!="ok",])
}

main <- function() {
    args<-parse_args(); outdir<-normalizePath(args[["output-dir"]],mustWork=TRUE)
    if(!(startsWith(outdir,"/tmp/miso-simple-effects-validation-") || startsWith(outdir,"/private/tmp/miso-simple-effects-validation-")) || length(list.files(outdir,all.files=TRUE,no..=TRUE))!=0L)
        stop("--output-dir must be a fresh empty directory under /tmp/miso-simple-effects-validation-*")
    antpath<-normalizePath(args[["ant-data"]],mustWork=TRUE)
    writeLines(c("Preregistered protocol: accepted validation-only protocol; no tuning.",
        "Support: independent observations, free permutations, no covariates; Euclidean or fourth-root Bray; sqrt.dist=FALSE; add=FALSE.",
        "Inference: subset-refitted conditional comparisons, not PRIMER-equivalent; PERMANOVA alone does not separate location/dispersion.",
        "Fixed run: 16 configurations, 200 null + 50 sensitivity replicates/config, 999 requested permutations/fit.",
        "Diagnostic: one-sided exact p<=.05 (>=16/200); acceptance hold p<=.05/56 (>=22/200); no reruns/tuning.",
        "Precision note: 10/200 rejections has an approximate 95% Clopper-Pearson interval .024-.090.",
        paste("R",getRversion(),"vegan",as.character(packageVersion("vegan")),"permute",as.character(packageVersion("permute"))),
        paste("Command:",paste(commandArgs(),collapse=" "))),file.path(outdir,"protocol.txt"))
    run_deterministic(outdir)
    sims<-run_simulations(outdir)
    ant<-run_ant(antpath,outdir)
    writeLines(c("Validation report",paste("Completed datasets:",nrow(sims$results)/6),
        "Supported simulation failures: 0 (unexpected failures stop immediately)",
        paste("Diagnostic warnings:",sum(sims$null$diagnostic_warning)),paste("Acceptance holds:",sum(sims$null$acceptance_hold)),
        paste("Ant retained rows:",ant$rows),paste("Ant duplicated labels:",paste(unique(ant$duplicate_labels),collapse="; ")),
        "Ant fits are unrestricted computational smoke only; exchangeability/ecological validity and PRIMER parity are unverified.",
        "No public simple-effects API was implemented or tested."),file.path(outdir,"summary.txt"))
    message("Validation outputs: ",outdir)
}
if(sys.nframe()==0L) main()
